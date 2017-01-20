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

import java.io.{File, FileNotFoundException, IOException, RandomAccessFile}
import java.net.{MalformedURLException, SocketAddress}

import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.handler.codec.http._
import io.netty.handler.codec.socks._
import io.netty.util.{AttributeKey, ReferenceCountUtil}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

import fm.http._
import fm.common.{Logging, URL}
import fm.common.Implicits._

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
  
  final case class SOCKSInit(msg: SocksInitRequest, promise: Promise[SocksInitResponse])
  final case class SOCKSAuth(msg: SocksAuthRequest, promise: Promise[SocksAuthResponse])
  final case class SOCKSConnect(msg: SocksCmdRequest, promise: Promise[SocksCmdResponse])
}

final class NettyHttpClientPipelineHandler(channelGroup: ChannelGroup, executionContext: ExecutionContext) extends ChannelInboundHandlerAdapter with ChannelOutboundHandler with Logging {
  import NettyHttpClientPipelineHandler._
  
  private[this] val id: Long = ID.incrementAndGet()
  private[this] implicit val executionCtx: ExecutionContext = executionContext
  
  @volatile private var pool: ChannelPool = null
  
  private[this] var socksInitPromise: Promise[SocksInitResponse] = null
  private[this] var socksAuthPromise: Promise[SocksAuthResponse] = null
  private[this] var socksConnectPromise: Promise[SocksCmdResponse] = null
  
  private[this] var responsePromise: Promise[AsyncResponse] = null
  private[this] var contentBuilder: LinkedHttpContentBuilder = null
  
  // If the Response has "Connection: close" then we set this
  private[this] var isConnectionClose: Boolean = false

  // This can be set to false when we close the channel due to the
  // isConnectionClose variable being true.
  // Note: We can't just rely on the isConnectionClose variable
  //       to manage this behavior because the server could still
  //       close the connection mid-response.
  private[this] var failPromisesOnChannelInactive: Boolean = true
  
  /** This is called once when a client connects to our server */
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    trace("channelActive")(ctx)

    channelGroup.add(ctx.channel())
    
    super.channelActive(ctx)
  }
   
  /** This is called once when a client disconnects from our server OR we close the connection */
  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    trace("channelInactive")(ctx)
    if (failPromisesOnChannelInactive) failPromises(new IOException("Channel Closed"))(ctx)
    super.channelInactive(ctx)
  }
  
  /** After calling a ctx.read() did we get a full message? */
  private[this] var gotMessage: Boolean = false
  
  /**
   * This gets called whenever a ctx.read() "completes" which may or may not have triggered a channelRead(...)
   * which means we need to keep track of whether or not we need to call ctx.read() to get an actual message
   * that we can handle with channelRead(...)
   */
  override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
    trace(s"channelReadComplete - gotMessage: $gotMessage")(ctx)
    
    if (gotMessage) gotMessage = false // We got a message.  Good!  Reset the Flag
    else ctx.read() // We didn't get a message so we need to trigger another ctx.read()
    
    super.channelReadComplete(ctx)
  }
  
  override protected def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    gotMessage = true
    if (logger.isTraceEnabled) trace("channelRead - "+msg.getClass.getSimpleName)(ctx)
    channelReadImpl(msg)(ctx)
    ReferenceCountUtil.release(msg)
  }
  
  protected def channelReadImpl(obj: AnyRef)(implicit ctx: ChannelHandlerContext): Unit = obj match {
    case response: SocksInitResponse =>
      require(null ne socksInitPromise, "Received a SocksInitResponse but the socksInitPromise is null!")
      socksInitPromise.trySuccess(response)
      socksInitPromise = null
      
    case response: SocksAuthResponse =>
      require(null ne socksAuthPromise, "Received a SocksAuthResponse but the socksAuthPromise is null!")
      socksAuthPromise.trySuccess(response)
      socksAuthPromise = null
      
    case response: SocksCmdResponse =>
      require(null ne socksConnectPromise, "Received a SocksCmdResponse but the socksConnectPromise is null!")
      socksConnectPromise.trySuccess(response)
      socksConnectPromise = null
    
    case response: HttpResponse =>      
      require(null eq contentBuilder, "Received an HttpResponse before the previous contentBuilder was completed!")     
      require(null ne responsePromise, "No promise to receive the HttpResponse")
      require(!obj.isInstanceOf[HttpContent], "Not Expecting HttpContent!")

      trace("HttpHeaders.isKeepAlive(response) => "+HttpHeaders.isKeepAlive(response))(ctx)

      if (!HttpHeaders.isKeepAlive(response)) {
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
          pool.release(ctx.channel())
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
  
  def writeRequest(uri: String, request: Request, promise: Promise[AsyncResponse], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("writeRequest")
    
    require(responsePromise eq null, "Expected responsePromise to be null")
    require(contentBuilder eq null, "Expected contentBuilder to be null")
    
    responsePromise = promise
    
    val version: HttpVersion = HttpVersion.HTTP_1_1
    
    request match {
      case full:  FullRequest  => sendFullRequest(promise, prepareRequest(full.toFullHttpRequest(version, uri)), channelPromise)
      case async: AsyncRequest => sendAsyncRequest(promise, prepareRequest(async.toHttpRequest(version, uri)), async.head, channelPromise)
      case file:  FileRequest  => sendFileRequest(promise, prepareRequest(file.toHttpRequest(version, uri)), file.file, channelPromise)
    }
    
    // Allow the HttpResponse message to be read
    ctx.read()
  }
  
  def writeSocksInit(cmd: SocksInitRequest, socksPromise: Promise[SocksInitResponse], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("writeSocksInit")
    writeSocksRequest(cmd, socksPromise, channelPromise)(p => socksInitPromise = p)
  }
  
  def writeSocksAuth(cmd: SocksAuthRequest, socksPromise: Promise[SocksAuthResponse], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("writeSocksAuth")
    writeSocksRequest(cmd, socksPromise, channelPromise)(p => socksAuthPromise = p)
  }
  
  def writeSocksConnect(cmd: SocksCmdRequest, socksPromise: Promise[SocksCmdResponse], channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("writeSocksConnect")
    writeSocksRequest(cmd, socksPromise, channelPromise)(p => socksConnectPromise = p)
  }
  
  private def writeSocksRequest[T <: SocksMessage, P](msg: SocksMessage, socksPromise: Promise[P], channelPromise: ChannelPromise)(setSocksPromise: Promise[P] => Unit)(implicit ctx: ChannelHandlerContext): Unit = {
    require(socksInitPromise eq null, "Expected socksInitPromise to be null")
    require(socksAuthPromise eq null, "Expected socksAuthPromise to be null")
    require(socksConnectPromise eq null, "Expected socksConnectPromise to be null")
    require(responsePromise eq null, "Expected responsePromise to be null")
    require(contentBuilder eq null, "Expected contentBuilder to be null")
    
    setSocksPromise(socksPromise)
    
    ctx.writeAndFlush(msg).onComplete{
      case Success(_) => channelPromise.setSuccess()
      case Failure(ex) => fail(socksPromise, ex, channelPromise)
    }
    
    // Allow the response to be read
    ctx.read()
  }
  
  /**
   * Set common headers for both Full & Async responses
   */
  private def prepareRequest[T <: HttpRequest](request: T): T = {
    val wantKeepAlive: Boolean = null != pool
    
    HttpHeaders.setKeepAlive(request, wantKeepAlive)
    
    request
  }
  
  private def sendFullRequest(promise: Promise[AsyncResponse], request: FullHttpRequest, channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendFullRequest")
    
    // Set the Content-Length since this is a full response which should have a known size
    HttpHeaders.setContentLength(request, request.content.readableBytes())
    
    ctx.writeAndFlush(request).onComplete{
      case Success(_) => channelPromise.setSuccess()
      case Failure(ex) => fail(promise, ex, channelPromise)
    }
  }
  
  private def sendAsyncRequest(promise: Promise[AsyncResponse], request: HttpRequest, head: LinkedHttpContent, channelPromise: ChannelPromise)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendAsyncRequest")
    
    // We don't know the size in advance since we'll be using: Transfer-Encoding: chunked 
    HttpHeaders.removeHeader(request, HttpHeaders.Names.CONTENT_LENGTH)
    HttpHeaders.setTransferEncodingChunked(request)
    
    ctx.writeAndFlush(request).onComplete{
      case Success(_) => sendChunk(promise, Some(head), channelPromise)
      case Failure(ex) => fail(promise, ex, channelPromise)
    }
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
    HttpHeaders.setContentLength(request, length)
    
    ctx.write(request)
    ctx.writeAndFlush(new DefaultFileRegion(raf.getChannel, 0, length)).flatMap{ _ => 
      ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT)
    }.onComplete{
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
    case SOCKSInit(msg, socksPromise) => writeSocksInit(msg, socksPromise, channelPromise)(ctx)
    case SOCKSAuth(msg, socksPromise) => writeSocksAuth(msg, socksPromise, channelPromise)(ctx)
    case SOCKSConnect(msg, socksPromise) => writeSocksConnect(msg, socksPromise, channelPromise)(ctx)
    case _ => throw new Exception("Invalid obj: "+obj)
  }
  
  def failPromises(cause: Throwable)(implicit ctx: ChannelHandlerContext): Unit = {
    trace(s"failPromises(socksInitPromise: $socksInitPromise, socksAuthPromise: $socksAuthPromise, socksConnectPromise: $socksConnectPromise, responsePromise: $responsePromise, contentBuilder: $contentBuilder)", cause)
    if (null != socksInitPromise) socksInitPromise.tryFailure(cause)
    if (null != socksAuthPromise) socksAuthPromise.tryFailure(cause)
    if (null != socksConnectPromise) socksConnectPromise.tryFailure(cause)
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