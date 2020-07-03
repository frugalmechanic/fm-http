/*
 * Copyright 2014 Frugal Mechanic (http://frugalmechanic.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fm.http.client

import fm.http._
import fm.common.Logging
import java.io.{File, FileNotFoundException, IOException, RandomAccessFile}
import java.net.SocketAddress
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.handler.codec.http._
import io.netty.util.ReferenceCountUtil
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object NettyHttpClientPipelineHandler {
  io.netty.handler.codec.http.multipart.DiskAttribute.deleteOnExitTemporaryFile = false  // DO NOT USE File.deleteOnExit() since it uses an append-only LinkedHashSet
  io.netty.handler.codec.http.multipart.DiskFileUpload.deleteOnExitTemporaryFile = false // DO NOT USE File.deleteOnExit() since it uses an append-only LinkedHashSet
  
  /**
   * A unique id for each connection (for debugging)
   */
  private val ID = new java.util.concurrent.atomic.AtomicLong
  
  def setChannelPool(ch: Channel, pool: ChannelPool): Unit = {
    ch.pipeline().get(classOf[NettyHttpClientPipelineHandler]).pool = pool
  }
  
  final case class URIRequestAndPromise(uri: String, request: Request, promise: Promise[AsyncResponse])
}

final class NettyHttpClientPipelineHandler(channelGroup: ChannelGroup) extends ChannelInboundHandlerAdapter with ChannelOutboundHandler with Logging {
  import NettyHttpClientPipelineHandler._
  
  private[this] val id: Long = ID.incrementAndGet()

  /** This is initialized by channelActive */
  private[this] implicit var executionContext: ExecutionContext = null
  
  @volatile private var pool: ChannelPool = null

  private[this] var responsePromise: Promise[AsyncResponse] = null
  private[this] var contentBuilder: LinkedHttpContentBuilder = null
  
  // If the Response has "Connection: close" then we set this
  private[this] var isConnectionClose: Boolean = false

  // Are we currently sending a request?
  private[this] var isSending: Boolean = false

  // This can be set to false when we close the channel due to the
  // isConnectionClose variable being true.
  // Note: We can't just rely on the isConnectionClose variable
  //       to manage this behavior because the server could still
  //       close the connection mid-response.
  private[this] var failPromisesOnChannelInactive: Boolean = true
  
  /** This is called once when a client connects to our server */
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    trace("channelActive")(ctx)

    // Use the EventLoop for our channel for all Future callbacks in this class
    executionContext = ExecutionContext.fromExecutor(ctx.channel().eventLoop())

    channelGroup.add(ctx.channel())
    
    super.channelActive(ctx)
  }
   
  /** This is called once when a client disconnects from our server OR we close the connection */
  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    trace("channelInactive")(ctx)
    if (failPromisesOnChannelInactive) failPromises(new IOException("Channel Closed"))(ctx)
    super.channelInactive(ctx)
  }
  
  override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
    trace(s"channelReadComplete")(ctx)

    super.channelReadComplete(ctx)
  }
  
  override protected def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    if (logger.isTraceEnabled) trace("channelRead - "+msg.getClass.getSimpleName)(ctx)
    channelReadImpl(msg)(ctx)
    ReferenceCountUtil.release(msg)
  }
  
  protected def channelReadImpl(obj: AnyRef)(implicit ctx: ChannelHandlerContext): Unit = obj match {
    case response: HttpResponse =>      
      require(null eq contentBuilder, "Received an HttpResponse before the previous contentBuilder was completed!")     
      require(null ne responsePromise, "No promise to receive the HttpResponse")
      require(!obj.isInstanceOf[HttpContent], "Not Expecting HttpContent!")

      trace("HttpUtil.isKeepAlive(response) => "+HttpUtil.isKeepAlive(response))(ctx)

      if (!HttpUtil.isKeepAlive(response)) {
        isConnectionClose = true
      }
      
      contentBuilder = LinkedHttpContentBuilder()
      channelReadHttpResponse(response, contentBuilder.future)
      
    case content: HttpContent =>
      require(null != contentBuilder, "Received an HttpContent but the contentBuilder is null!")
      contentBuilder += content
      
      if (contentBuilder.isDone) {
        trace("channelReadImpl - contentBuilder.isDone")
        
        contentBuilder = null
        
        if (isConnectionClose || null == pool) {
          trace("channelReadImpl - ctx.close()")
          // We need to make sure we don't fail promises in the channelInactive
          // method since we expect the channel to be closed.
          failPromisesOnChannelInactive = false
          ctx.close()
        } else {
          if (isSending) {
            trace("channelReadImpl - isSending - ctx.close()")
            // If we have received a response while we are still sending then we do not want to release this back
            // to the pool.  We should probably just close the channel (the server side is probably closing it anyways)
            ctx.close()
          } else {
            trace("channelReadImpl - pool.release()")
            pool.release(ctx.channel())
          }
        }
      }
  }
  
  private def channelReadHttpResponse(nettyResponse: HttpResponse, content: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext): Unit = {
    require(null ne responsePromise, "No promise to receive the HttpResponse")
    
    val contentReader: LinkedHttpContentReader = LinkedHttpContentReader(need100Continue = false, content)
    
    val response: AsyncResponse = new AsyncResponse(nettyResponse, contentReader)
    
    responsePromise.trySuccess(response)
    responsePromise = null
  }

  private def writeRequest(uri: String, request: Request, promise: Promise[AsyncResponse], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("writeRequest")
    
    require(responsePromise eq null, "Expected responsePromise to be null")
    require(contentBuilder eq null, "Expected contentBuilder to be null")
    
    responsePromise = promise
    
    val version: HttpVersion = HttpVersion.HTTP_1_1

    isSending = true

    request match {
      case full:  FullRequest  => sendFullRequest(promise, prepareRequest(full.toFullHttpRequest(version, uri)), channelPromise)
      case async: AsyncRequest => sendAsyncRequest(promise, prepareRequest(async.toHttpRequest(version, uri)), async.head, channelPromise)
      case file:  FileRequest  => sendFileRequest(promise, prepareRequest(file.toHttpRequest(version, uri)), file.file, channelPromise)
    }

    channelPromise.onComplete{
      case Success(_) => isSending = false
      case Failure(_) => // Do nothing
    }
    
    // Allow the HttpResponse message to be read
    ctx.read()
  }

  /**
   * Set common headers for both Full & Async responses
   */
  private def prepareRequest[T <: HttpRequest](request: T): T = {
    val wantKeepAlive: Boolean = null != pool

    HttpUtil.setKeepAlive(request, wantKeepAlive)
    
    request
  }
  
  private def sendFullRequest(promise: Promise[AsyncResponse], request: FullHttpRequest, channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendFullRequest")
    
    // Set the Content-Length since this is a full response which should have a known size
    HttpUtil.setContentLength(request, request.content.readableBytes())
    HttpUtil.setTransferEncodingChunked(request, false)
    
    ctx.writeAndFlush(request).onComplete{
      case Success(_) => channelPromise.setSuccess()
      case Failure(ex) => fail(promise, ex, channelPromise)
    }
  }
  
  private def sendAsyncRequest(promise: Promise[AsyncResponse], request: HttpRequest, head: Future[Option[LinkedHttpContent]], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendAsyncRequest")
    
    // We don't know the size in advance since we'll be using: Transfer-Encoding: chunked
    request.headers().remove(HttpHeaderNames.CONTENT_LENGTH)
    HttpUtil.setTransferEncodingChunked(request, true)
    
    ctx.writeAndFlush(request).onComplete{ onChunkSendComplete(promise, _, head, channelPromise) }
  }
  
  private def sendChunk(promise: Promise[AsyncResponse], chunk: Try[Option[LinkedHttpContent]], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendChunk - Try wrapped")
    
    chunk match {
      case Success(optContent) => sendChunk(promise, optContent, channelPromise)
      case Failure(ex) => fail(promise, ex, channelPromise)
    }
  }
  
  private def sendChunk(promise: Promise[AsyncResponse], chunk: Option[LinkedHttpContent], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendChunk - unwrapped")
    
    chunk match {
      case Some(content) => require(content.nonEmpty, "Empty Buffer?!"); ctx.writeAndFlush(content).onComplete{ onChunkSendComplete(promise, _, content.tail, channelPromise) }
      case None => ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{
        case Success(_) => channelPromise.setSuccess()
        case Failure(ex) => fail(promise, ex, channelPromise)
      }
    }
  }
  
  private def onChunkSendComplete(promise: Promise[AsyncResponse], res: Try[Void], nextChunk: Future[Option[LinkedHttpContent]], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("onChunkSendComplete")
    
    res match {
      case Success(_) => nextChunk.onComplete{ sendChunk(promise, _, channelPromise) }
      case Failure(ex) => 
        trace("onChunkSendComplete - FAILURE", ex)
        fail(promise, ex, channelPromise)
    }
  }
  
  private def sendFileRequest(promise: Promise[AsyncResponse], request: HttpRequest, file: File, channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendFileRequest")
    
    if (!file.isFile || !file.canRead) {
      fail(promise, new FileNotFoundException("Missing File: "+file), channelPromise)
      return
    }
    
    val raf: RandomAccessFile = new RandomAccessFile(file, "r")
    val length: Long = raf.length()
    
    // Set the Content-Length since we know the length of the file
    HttpUtil.setContentLength(request, length)
    HttpUtil.setTransferEncodingChunked(request, false)
    
    ctx.write(request, ctx.voidPromise())
    ctx.write(new DefaultFileRegion(raf.getChannel, 0, length), ctx.voidPromise())

    ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{
      case Success(_) => channelPromise.setSuccess()
      case Failure(ex) => fail(promise, ex, channelPromise)
    }
  }
  
  private def fail(promise: Promise[_], ex: Throwable, channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("fail(): "+ex)
    promise.tryFailure(ex)
    channelPromise.setFailure(ex)
    ctx.close()
  }
  
  //
  // ChannelOutboundHandler Methods
  //
  def bind(ctx: ChannelHandlerContext, localAddress: SocketAddress, promise: ChannelPromise): Unit = ctx.bind(localAddress, promise)
  def close(ctx: ChannelHandlerContext, promise: ChannelPromise): Unit = ctx.close(promise)
  def connect(ctx: ChannelHandlerContext, remoteAddress: SocketAddress, localAddress: SocketAddress, promise: ChannelPromise): Unit = ctx.connect(remoteAddress, localAddress, promise)
  def deregister(ctx: ChannelHandlerContext, promise: ChannelPromise): Unit = ctx.deregister(promise)
  def disconnect(ctx: ChannelHandlerContext, promise: ChannelPromise): Unit = ctx.disconnect(promise)
  def read(ctx: ChannelHandlerContext): Unit = ctx.read()
  
  def flush(ctx: ChannelHandlerContext): Unit = {
    // Empty because for messages that we write() we already call writeAndFlush()
  }
  
  def write(ctx: ChannelHandlerContext, obj: AnyRef, channelPromise: ChannelPromise): Unit = obj match {
    case URIRequestAndPromise(uri, request, promise) => writeRequest(uri, request, promise, channelPromise)(ctx)
    case _ => throw new Exception("Invalid obj: "+obj)
  }
  
  def failPromises(cause: Throwable)(implicit ctx: ChannelHandlerContext): Unit = {
    trace(s"failPromises(responsePromise: $responsePromise, contentBuilder: $contentBuilder)", cause)
    if (null != responsePromise) responsePromise.tryFailure(cause)
    if (null != contentBuilder) contentBuilder += cause
  }
  
  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    logger.error(s"$id - exceptionCaught - ${ctx.channel}", cause)
    failPromises(cause)(ctx)
    ctx.close()
  }
  
  private def trace(name: String, ex: Throwable = null)(implicit ctx: ChannelHandlerContext): Unit = {
    if (logger.isTraceEnabled) logger.trace(s"$id - $name - ${ctx.channel}", ex)
  }
}