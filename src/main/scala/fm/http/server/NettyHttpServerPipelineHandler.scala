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
package fm.http.server

import fm.common.{IP, Logging}
import fm.common.Implicits._
import fm.http._

import io.netty.buffer.Unpooled
import io.netty.channel.{Channel, ChannelHandlerContext, DefaultFileRegion, SimpleChannelInboundHandler}
import io.netty.channel.group.ChannelGroup
import io.netty.handler.codec.http._
import io.netty.handler.stream.{ChunkedFile, ChunkedStream}
import io.netty.util.{AttributeKey, CharsetUtil}

import java.io.{File, FileNotFoundException, InputStream, RandomAccessFile}
import java.util.Date
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object NettyHttpServerPipelineHandler {
  io.netty.handler.codec.http.multipart.DiskAttribute.deleteOnExitTemporaryFile = false  // DO NOT USE File.deleteOnExit() since it uses an append-only LinkedHashSet
  io.netty.handler.codec.http.multipart.DiskFileUpload.deleteOnExitTemporaryFile = false // DO NOT USE File.deleteOnExit() since it uses an append-only LinkedHashSet
  
  /**
   * A unique id for each connection (for debugging)
   */
  private val ID = new java.util.concurrent.atomic.AtomicLong
  
  /**
   * The maximum number of requests that we will process per connection
   */
  private val MaxRequestsPerConnection: Long = 128
  
  /**
   * For marking if a channel is currently processing a request
   */
  private val ProcessingRequestKey = AttributeKey.valueOf[Boolean]("NettyHttpServerPipelineHandler.ProcessingRequest")
  
  /**
   * Is this channel currently processing a request?
   */
  def isProcessingRequest(ch: Channel): Boolean = Option(ch.attr(ProcessingRequestKey).get).getOrElse(false)
}

/**
 * Each connection has once instance of this Handler created which means it can be used to track state if needed.
 */
final class NettyHttpServerPipelineHandler(channelGroup: ChannelGroup, executionContext: ExecutionContext, router: RequestRouter) extends SimpleChannelInboundHandler[HttpObject] with Logging {
  import NettyHttpServerPipelineHandler._
  
  private[this] val id: Long = ID.incrementAndGet()
  private[this] implicit val executionCtx: ExecutionContext = executionContext
  
  /** The number of requests that we've handled for this connection */
  private[this] var numRequestsHandled: Long = 0L
  
  /** This is called once when a client connects to our server */
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    trace("channelActive")(ctx)
    
    // Allow the first message to be read
    ctx.read()

    channelGroup.add(ctx.channel())
    
    super.channelActive(ctx)
  }
   
  /** This is called once when a client disconnects from our server OR we close the connection */
  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    trace("channelInactive")(ctx)
    super.channelInactive(ctx)
  }
  
  override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
    trace("channelReadComplete")(ctx)
    super.channelReadComplete(ctx)
  }
  
  private[this] var contentBuilder: LinkedHttpContentBuilder = null
  
  protected def channelRead0(ctx: ChannelHandlerContext, obj: HttpObject): Unit = obj match {
//    // This is a complete HttpRequest -- NOT CURRENTLY USED SINCE THIS CONDITION DOESN'T GET TRIGGERED UNDER NORMAL USE
//    //                                   HOWEVER, I did see this get triggered when trying to connect to the server using
//    //                                   HTTPS so maybe this could we be useful at some point to detect HTTPS connection
//    //                                   trying to talk to our HTTPS server.  OR maybe we just need to add an HTTPS handler
//    case request: FullHttpRequest =>
//      if (logger.isTraceEnabled) trace("channelRead0 - "+obj.getClass.getSimpleName)(ctx)
//      
//      require(null == contentBuilder, "Received a FullHttpRequest before the previous contentBuilder was completed!")
//      
//      channelReadHttpRequest(request, Future.successful(Option(request.content()).map{ LinkedHttpContent(_) }))(ctx)
//    
    // This is the beginning of a request
    case request: HttpRequest =>
      if (logger.isTraceEnabled) trace("channelRead0 - "+obj.getClass.getSimpleName)(ctx)
      
      require(null == contentBuilder, "Received an HttpRequest before the previous contentBuilder was completed!")
      require(!obj.isInstanceOf[HttpContent], "Not Expecting HttpContent!")
      
      contentBuilder = LinkedHttpContentBuilder()
      channelReadHttpRequest(request, contentBuilder.future)(ctx)
    
    // This is a chunk of content that goes with the HttpRequest
    case content: HttpContent =>
      if (logger.isTraceEnabled) trace("channelRead0 - "+obj.getClass.getSimpleName)(ctx)
      require(null != contentBuilder, "Received an HttpContent but the contentBuilder is null!")
      contentBuilder += content
      if (contentBuilder.isDone) contentBuilder = null
  }
  
  protected def channelReadHttpRequest(nettyRequest: HttpRequest, content: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext): Unit = {
    trace("channelReadHttpRequest")
    
    numRequestsHandled += 1
    
    ctx.channel().attr(ProcessingRequestKey).set(true)
    
    val (request: Request, response: Future[Response]) = handle(nettyRequest, content)
    
    val wantKeepAlive: Boolean = HttpHeaders.isKeepAlive(nettyRequest) && numRequestsHandled < MaxRequestsPerConnection
    
    // The Response Version should match the request version
    val version: HttpVersion = nettyRequest.getProtocolVersion()
    
    // TODO: handle HEAD requests where we don't want to send back the body
    response.recover { case ex: Throwable =>
      logger.error("Caught Exception waiting for Response Future - Sending Error Response", ex)
      sendFullResponse(request, prepareResponse(request, makeErrorResponse(Status.INTERNAL_SERVER_ERROR).toFullHttpResponse(version), wantKeepAlive))
    }.onSuccess { res: Response => res match {
      case full:  FullResponse             => sendFullResponse(request, prepareResponse(request, full.toFullHttpResponse(version), wantKeepAlive))
      case async: AsyncResponse            => sendAsyncResponse(request, prepareResponse(request, async.toHttpResponse(version), wantKeepAlive), async.head)
      case input: InputStreamResponse      => sendInputStreamResponse(request, prepareResponse(request, input.toHttpResponse(version), wantKeepAlive), input.input, input.length)
      case file:  RandomAccessFileResponse => sendRandomAccessFileResponse(request, prepareResponse(request, file.toHttpResponse(version), wantKeepAlive), file.file)
      case file:  FileResponse             =>
        try {
          sendFileResponse(request, prepareResponse(request, file.toHttpResponse(version), wantKeepAlive), file.file)
        } catch {
          case ex: FileNotFoundException => sendFullResponse(request, prepareResponse(request, makeErrorResponse(Status.NOT_FOUND).toFullHttpResponse(version), wantKeepAlive))
        }
    }}
  }
  
  /**
   * Handles routing of the request.  If no handler exists then we return a 404 response.
   * If an exception is thrown while trying to handle the request then a 500 response is returned.
   * 
   * Note: If these 404/500 pages need to be customized then the router should return a 404 handler
   * and the handler should have exception wrapping to handle exceptions.
   */
  private def handle(request: HttpRequest, content: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext): (Request, Future[Response]) = {    
    
    val remoteIp: IP = remoteIPForRequest(request)
     
    val is100ContinueExpected: Boolean = HttpHeaders.is100ContinueExpected(request)
    
    val contentReader: LinkedHttpContentReader = LinkedHttpContentReader(is100ContinueExpected, content)
    val r: Request = Request(remoteIp, request, contentReader)
    
    val future: Future[Response] = router.lookup(r) match {
      case Some(handler) => 
        try {
          handler(r)
        } catch {
          case ex: Exception =>
            logger.error(s"Caught exception handling request: request", ex)
            Future.successful(makeErrorResponse(Status.INTERNAL_SERVER_ERROR))
        }
      
      case None => Future.successful(makeErrorResponse(Status.NOT_FOUND))
    }
    
    (r, future)
  }
  
  /**
   * TODO: test this with HAProxy/Apache/Nginx
   */
  private def remoteIPForRequest(request: HttpRequest)(implicit ctx: ChannelHandlerContext): IP = {
    import scala.collection.JavaConverters._
    import java.net.InetSocketAddress
    
    Seq("FM-Remote-IP", "X-Forwarded-For").flatMap{ name: String =>
      // We only care about the last value for the Header (since HAProxy/Apache appends it's value)
      request.headers().getAll(name).asScala.flatMap{ IP.findAllIPsIn }.lastOption
    }.headOption.getOrElse{ IP(ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress].getAddress) }

  }

  /**
   * Set common headers for both Full & Async responses
   */
  private def prepareResponse[T <: HttpResponse](request: Request, response: T, wantKeepAlive: Boolean)(implicit ctx: ChannelHandlerContext): T = {
    HttpHeaders.setKeepAlive(response, wantKeepAlive)
    
    // Set the "Date" HTTP Header if it isn't already set
    if (null == response.headers().get(HttpHeaders.Names.DATE)) HttpHeaders.setDate(response, new Date)
    
    if (!request.isContentFullyRead && response.getStatus() === HttpResponseStatus.OK) {
      logger.warn("Sending a 200 response but the request body has not been fully read: "+request)
    }
    
    response
  }
  
  /**
   * If an error occurs before we've send the Response Headers then this is called to send something appropriate back
   */
  private def makeErrorResponse(status: Status): FullResponse = {
    FullResponse(status, Headers(HttpHeaders.Names.CONTENT_TYPE -> "text/plain; charset=UTF-8"), Unpooled.copiedBuffer(s"${status.code} ${status.msg}", CharsetUtil.UTF_8))
  }
  
  /**
   * For sending an InputStream-based response
   */
  private def sendInputStreamResponse(request: Request, response: HttpResponse, input: InputStream, length: Option[Long])(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendInputStreamResponse")
    
    // Set the Content-Length if we know the length of the InputStream
    if (includeContentLength(request, response)) length.foreach { HttpHeaders.setContentLength(response, _) }
    
    // The NettyContentCompressor can't handle a ChunkedFile (which is a ChunkedInput[ByteBuf]) 
    // so we wrap it in HttpContentChunkedInput to turn it into a ChunkedInput[HttpContent]
    val obj: HttpContentChunkedInput = HttpContentChunkedInput(new ChunkedStream(input))
    
    ctx.write(response)
    ctx.writeAndFlush(obj).onComplete { res: Try[Void] =>
      if (res.isFailure) onResponseComplete(request, res, HttpHeaders.isKeepAlive(response))
      else ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{ onResponseComplete(request, _, HttpHeaders.isKeepAlive(response)) }
    }
  }
  
  /**
   * For sending a File-based response
   */
  private def sendFileResponse(request: Request, response: HttpResponse, file: File)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendFileResponse")
    
    if (!file.isFile || !file.canRead) throw new FileNotFoundException("Missing File: "+file)
    
    val raf: RandomAccessFile = new RandomAccessFile(file, "r")

    sendRandomAccessFileResponse(request, response, raf)
  }

  /**
   * For sending a File-based response
   */
  private def sendRandomAccessFileResponse(request: Request, response: HttpResponse, raf: RandomAccessFile)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendRandomAccessFileResponse")

    val length: Long = raf.length()

    // Set the Content-Length since we know the length of the file
    if (includeContentLength(request, response)) HttpHeaders.setContentLength(response, length)

    // If we might GZIP/DEFLATE this content then we can't use sendfile since it would
    // bypass the NettyContentCompressor which would have already modified the Content-Encoding
    val useSendFile: Boolean = !NettyContentCompressor.isCompressable(response)

    trace(s"sendRandomAccessFileResponse(sendFile => $useSendFile)")

    val obj: AnyRef = if (useSendFile) {
      new DefaultFileRegion(raf.getChannel, 0, length)
    } else {
      // The NettyContentCompressor can't handle a ChunkedFile (which is a ChunkedInput[ByteBuf])
      // so we wrap it in HttpContentChunkedInput to turn it into a ChunkedInput[HttpContent]
      HttpContentChunkedInput(new ChunkedFile(raf, 0, length, 8192))
    }

    ctx.write(response)
    ctx.writeAndFlush(obj).onComplete { res: Try[Void] =>
      // Make sure our RandomAccessFile got closed
      raf.close()

      if (res.isFailure) onResponseComplete(request, res, HttpHeaders.isKeepAlive(response))
      else ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{ onResponseComplete(request, _, HttpHeaders.isKeepAlive(response)) }
    }
  }
  
  /**
   * For sending a Full Http Response
   */
  private def sendFullResponse(request: Request, response: FullHttpResponse)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendFullResponse")
    
    // Set the Content-Length since this is a full response which should have a known size
    if (includeContentLength(request, response)) HttpHeaders.setContentLength(response, response.content.readableBytes())
    
    ctx.writeAndFlush(response).onComplete{ onResponseComplete(request, _, HttpHeaders.isKeepAlive(response)) }
  }
  
  /**
   * For sending a chunked http response
   */
  private def sendAsyncResponse(request: Request, response: HttpResponse, head: LinkedHttpContent)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendAsyncResponse")
    
    // We don't know the size in advance since we'll be using: Transfer-Encoding: chunked 
    HttpHeaders.removeHeader(response, HttpHeaders.Names.CONTENT_LENGTH)
    HttpHeaders.setTransferEncodingChunked(response)
    
    // The ChannelFuture return from ctx.write(...) doesn't complete unless you eventually call flush.
    // So we write the response headers and immediately writeAndFlush the first chunk.
    ctx.write(response)
    sendChunk(request, Some(head), HttpHeaders.isKeepAlive(response))
  }
  
  /**
   * For triggering the send of a single chunk that is wrapped in a Try
   */
  private def sendChunk(request: Request, chunk: Try[Option[LinkedHttpContent]], isKeepAlive: Boolean)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendChunk - Try wrapped")
    
    chunk match {
      case Success(optContent) => sendChunk(request, optContent, isKeepAlive)
      case Failure(ex)         => handleFailedChunkFuture(request, ex)
    }
  }
  
  /**
   * For triggering the send of a single chunk
   */
  private def sendChunk(request: Request, chunk: Option[LinkedHttpContent], isKeepAlive: Boolean)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendChunk - unwrapped")
    
    chunk match {
      //case Some(content) => require(content.nonEmpty, "Empty Buffer?!"); ctx.writeAndFlush(content).onComplete{ onChunkSendComplete(request, _, content.tail, isKeepAlive) }
      case Some(content) => ctx.writeAndFlush(content).onComplete{ onChunkSendComplete(request, _, content.tail, isKeepAlive) }
      case None          => ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{ onResponseComplete(request, _, isKeepAlive) }
    }
  }
  
  private def onChunkSendComplete(request: Request, res: Try[Void], nextChunk: Future[Option[LinkedHttpContent]], isKeepAlive: Boolean)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("onChunkSendComplete")
    
    res match {
      case Success(_)  => nextChunk.onComplete{ sendChunk(request, _, isKeepAlive) }
      case Failure(ex) => trace("onChunkSendComplete - FAILURE", ex)
    }
  }
  
  /**
   * If a chunk Future returns a Failure this method will get called
   */
  private def handleFailedChunkFuture(request: Request, ex: Throwable)(implicit ctx: ChannelHandlerContext): Unit = {
    logger.error(s"$id - Chunk Future returned Failure() - ${ctx.channel}", ex)
    request.close(ex)
    ctx.close()
  }
  
  /**
   * Called once we've completed sending the response
   */
  private def onResponseComplete(request: Request, res: Try[Void], isKeepAlive: Boolean)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("onResponseComplete")
    
    // We are no longer processing this request
    ctx.channel().attr(ProcessingRequestKey).set(false)
    
    // Only read the next request if we want Keep-Alive and the request body has been fully read,
    // otherwise we will close the connection.
    val readNextRequest: Boolean = isKeepAlive && request.isContentFullyRead
    
    trace("Request.close()")
    request.close()
    
    res match {
      // If this is a keep-alive connection then we allow reading of the next request otherwise we close the connection
      case Success(_) => if (readNextRequest) ctx.read() else ctx.close()
      case Failure(ex) => trace("onResponseComplete - FAILURE", ex)
    }
  }
  
  /** 
   *  Can this type of response include a ContentLength?
   *  
   *  http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.3
   */
  private def includeContentLength(request: Request, response: HttpResponse): Boolean = {
    if (request.method === HttpMethod.HEAD) return false
    
    response.getStatus.code match {
      case 100 | 101 | 102 => false
      case 204 => false
      case 304 => false
      case _ => true
    }
  }
  
  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    logger.error(s"$id - exceptionCaught - ${ctx.channel}", cause)
    if (null != contentBuilder) contentBuilder += cause
    ctx.close()
  }
  
  private def trace(name: String, ex: Throwable = null)(implicit ctx: ChannelHandlerContext): Unit = {
    if (logger.isTraceEnabled) logger.trace(s"$id - $name - ${ctx.channel}", ex)
  }
}