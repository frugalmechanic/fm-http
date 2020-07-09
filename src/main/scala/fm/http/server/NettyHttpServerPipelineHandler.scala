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

import fm.common.{IP, InvalidIPException, Logging}
import fm.common.Implicits._
import fm.http._
import io.netty.buffer.Unpooled
import io.netty.channel.{Channel, ChannelHandlerContext, DefaultFileRegion, SimpleChannelInboundHandler}
import io.netty.channel.group.ChannelGroup
import io.netty.handler.codec.http._
import io.netty.handler.stream.{ChunkedFile, ChunkedStream}
import io.netty.util.{AttributeKey, CharsetUtil}
import java.io.{File, FileNotFoundException, InputStream, RandomAccessFile}
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.AtomicLong
import java.util.Date
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object NettyHttpServerPipelineHandler {
  io.netty.handler.codec.http.multipart.DiskAttribute.deleteOnExitTemporaryFile = false  // DO NOT USE File.deleteOnExit() since it uses an append-only LinkedHashSet
  io.netty.handler.codec.http.multipart.DiskFileUpload.deleteOnExitTemporaryFile = false // DO NOT USE File.deleteOnExit() since it uses an append-only LinkedHashSet
  
  /**
   * A unique id for each connection (for debugging)
   */
  private val ID: AtomicLong = new AtomicLong()
  
  /**
   * For marking if a channel is currently processing a request
   */
  private val ProcessingRequestKey = AttributeKey.valueOf[Boolean]("NettyHttpServerPipelineHandler.ProcessingRequest")
  
  /**
   * Is this channel currently processing a request?
   */
  def isProcessingRequest(ch: Channel): Boolean = Option(ch.attr(ProcessingRequestKey).get).getOrElse(false)

  private class CloseContextRunnable(ctx: ChannelHandlerContext) extends Runnable {
    override def run(): Unit = ctx.close()
  }
}

/**
 * Each connection has one instance of this Handler created which means it can be used to track state if needed.
 */
final class NettyHttpServerPipelineHandler(
  channelGroup: ChannelGroup,
  router: RequestRouter,
  options: HttpServerOptions
) extends SimpleChannelInboundHandler[HttpObject] with Logging {
  import NettyHttpServerPipelineHandler._
  
  private[this] val id: Long = ID.incrementAndGet()
  
  /** The number of requests that we've handled for this connection */
  private[this] var numRequestsHandled: Long = 0L

  private[this] var contentBuilder: LinkedHttpContentBuilder = null

  /** This is initialized by channelActive */
  private[this] implicit var executionContext: ExecutionContext = null

  /** This is called once when a client connects to our server */
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    trace("channelActive")(ctx)

    // Use the EventLoop for our channel for all Future callbacks in this class
    executionContext = ExecutionContext.fromExecutor(ctx.channel().eventLoop())
    
    // Allow the first message to be read
    if (logger.isTraceEnabled) logger.trace("ctx.read()")
    ctx.read()

    // JavaDocs for ChannelGroup (https://netty.io/4.1/api/io/netty/channel/group/ChannelGroup.html) state:
    // "A closed Channel is automatically removed from the collection, so that you don't need to worry about the life cycle of the added Channel."
    channelGroup.add(ctx.channel())
    
    super.channelActive(ctx)
  }
   
  /** This is called once when a client disconnects from our server OR we close the connection */
  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    trace("channelInactive")(ctx)
    if (null != contentBuilder && !contentBuilder.isDone) contentBuilder += (new ClosedChannelException())
    super.channelInactive(ctx)
  }
  
  override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
    trace("channelReadComplete")(ctx)
    super.channelReadComplete(ctx)
  }
  
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
    
    // The Response Version should match the request version
    val version: HttpVersion = nettyRequest.protocolVersion()
    
    // TODO: handle HEAD requests where we don't want to send back the body
    response.recover { case ex: Throwable =>
      logger.error("Caught Exception waiting for Response Future - Sending Error Response", ex)
      makeErrorResponse(Status.INTERNAL_SERVER_ERROR)
    }.foreach { res: Response =>
      // We only want keep-alive if:
      //   1. The request wants it (either via HTTP 1.1 or an explicit "Connection: keep-alive" request header)
      //   2. We are below our maxRequestsPerConnection
      //   3. We have fully read the request.  If we have not fully read the request then something went wrong and the
      //      connection will be automatically closed by us.  So we should let the client know in the response.
      val wantKeepAlive: Boolean = HttpUtil.isKeepAlive(nettyRequest) && numRequestsHandled < options.maxRequestsPerConnection && request.isContentFullyRead

      trace(s"channelReadHttpRequest - wantKeepAlive: $wantKeepAlive, HttpUtil.isKeepAlive(nettyRequest): ${HttpUtil.isKeepAlive(nettyRequest)}, numRequestsHandled < options.maxRequestsPerConnection: ${numRequestsHandled < options.maxRequestsPerConnection}, request.isContentFullyRead: ${request.isContentFullyRead}")

      res match {
        case full:  FullResponse             => sendFullResponse(request, prepareResponse(request, full.toFullHttpResponse(version), wantKeepAlive))
        case async: AsyncResponse            => sendAsyncResponse(request, prepareResponse(request, async.toHttpResponse(version), wantKeepAlive), async.head)
        case input: InputStreamResponse      => sendInputStreamResponse(request, prepareResponse(request, input.toHttpResponse(version), wantKeepAlive), input.input, input.length)
        case file:  RandomAccessFileResponse => sendRandomAccessFileResponse(request, prepareResponse(request, file.toHttpResponse(version), wantKeepAlive), file.file)
        case file:  FileResponse             =>
          try {
            sendFileResponse(request, prepareResponse(request, file.toHttpResponse(version), wantKeepAlive), file.file)
          } catch {
            case _: FileNotFoundException => sendFullResponse(request, prepareResponse(request, makeErrorResponse(Status.NOT_FOUND).toFullHttpResponse(version), wantKeepAlive))
          }
      }
    }
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
     
    val is100ContinueExpected: Boolean = HttpUtil.is100ContinueExpected(request)
    
    val contentReader: LinkedHttpContentReader = LinkedHttpContentReader(is100ContinueExpected, content)
    val r: Request = Request(remoteIp, request, contentReader)

    val handlerExecutionContext: ExecutionContext = options.requestHandlerExecutionContextProvider match {
      case Some(provider) => provider(r)
      case None => implicitly // Use the default ExecutionContext everything else in this class uses
    }

    val handlerPromise: Promise[Response] = Promise()

    // Note: User code (RequestRouter and RequestHandler) are executed in our handlerExecutionContext
    handlerExecutionContext.execute {
      new Runnable() {
        override def run(): Unit = {
          val future: Future[Response] = router.lookup(r) match {
            case Some(handler) =>
              try {
                handler(r)(handlerExecutionContext)
              } catch {
                case ex: Exception =>
                  logger.error(s"Caught exception handling request: request", ex)
                  Future.successful(makeErrorResponse(Status.INTERNAL_SERVER_ERROR))
              }

            case None => Future.successful(makeErrorResponse(Status.NOT_FOUND))
          }

          handlerPromise.completeWith(future)
        }
      }
    }

    (r, handlerPromise.future)
  }
  
  private def remoteIPForRequest(request: HttpRequest)(implicit ctx: ChannelHandlerContext): IP = {
    import scala.collection.JavaConverters._
    import java.net.InetSocketAddress

    val headers: HttpHeaders = request.headers()

    options.clientIPLookupSpecs.findMapped{ spec: HttpServerOptions.ClientIPLookupSpec =>
      val hasRequiredHeaderOrEmpty: Boolean = spec.requiredHeaderAndValue.map{ case (name: String, value: String) =>
        // If the requiredHeaderAndValue is set then we require it to be part of the headers
        headers.containsValue(name, value, false /* ignoreCase */)
      }.getOrElse(true) // If requiredHeaderAndValue is not set then we are okay

      if (hasRequiredHeaderOrEmpty) {
        val ips: IndexedSeq[IP] = headers.getAll(spec.headerName).asScala.flatMap{ IP.findAllIPsIn }.toIndexedSeq

        import HttpServerOptions.ClientIPHeaderValueToUse._

        spec.valueToUse match {
          case First                => ips.headOption
          case Last                 => ips.lastOption
          case OffsetFromFirst(idx) => if (ips.length > idx) Some(ips(idx)) else ips.headOption
          case OffsetFromLast(idx)  => if (ips.length > idx) Some(ips(ips.length - 1 - idx)) else ips.lastOption
        }
      } else {
        None
      }
    }.getOrElse{
      try {
        // Default to the IP of whoever is actually connecting to us
        IP(ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress].getAddress)
      } catch {
        // Note: This currently fails on IPv6 Addresses since the fm.common.IP class does not support them.  In that
        //       case we currently just return IP.empty (0) as the IP to prevent the server from completely failing
        case _: InvalidIPException => IP.empty
      }
    }

  }

  /**
   * Set common headers for both Full & Async responses
   */
  private def prepareResponse[T <: HttpResponse](request: Request, response: T, wantKeepAlive: Boolean)(implicit ctx: ChannelHandlerContext): T = {
    HttpUtil.setKeepAlive(response, wantKeepAlive)
    
    // Set the "Date" HTTP Header if it isn't already set
    if (null == response.headers().get(HttpHeaderNames.DATE)) response.headers().set(HttpHeaderNames.DATE, new Date)
    
    if (!request.isContentFullyRead && response.status() === HttpResponseStatus.OK) {
      logger.warn("Sending a 200 response but the request body has not been fully read: "+request)
    }

    // Add in an X-Request-Id header if configured in the HttpServerOptions
    options.requestIdResponseHeader.foreach { header: String =>
      response.headers().add(header, request.id.toHex())
    }

    // Add in any overrides
    Response.headerModifications.get(request) match {
      case None =>
        // Nothing to do

      case Some(builder) =>
        // Wrap the Netty HttpHeaders in our MutableHeaders
        val headers: MutableHeaders = MutableHeaders(response.headers())

        // Apply any mutations to the headers (which will modify the underlying netty HttpHeaders)
        builder.result.foreach{ f: (MutableHeaders => Unit) => f(headers) }
    }

    response
  }
  
  /**
   * If an error occurs before we've send the Response Headers then this is called to send something appropriate back
   */
  private def makeErrorResponse(status: Status): FullResponse = {
    FullResponse(status, Headers(HttpHeaderNames.CONTENT_TYPE -> "text/plain; charset=UTF-8"), Unpooled.copiedBuffer(s"${status.code} ${status.msg}", CharsetUtil.UTF_8))
  }
  
  /**
   * For sending an InputStream-based response
   */
  private def sendInputStreamResponse(request: Request, response: HttpResponse, input: InputStream, length: Option[Long])(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendInputStreamResponse")

    // Set the Content-Length if we know the length of the InputStream
    if (includeContentLength(request, response)) {
      length match {
        case Some(len) =>
          HttpUtil.setContentLength(response, len)
          HttpUtil.setTransferEncodingChunked(response, false)

        case None =>
          // Length is not known so set chunked
          response.headers().remove(HttpHeaderNames.CONTENT_LENGTH)
          HttpUtil.setTransferEncodingChunked(response, true)
      }

    }

    // The NettyContentCompressor can't handle a ChunkedFile (which is a ChunkedInput[ByteBuf]) 
    // so we wrap it in HttpContentChunkedInput to turn it into a ChunkedInput[HttpContent]
    val obj: HttpContentChunkedInput = HttpContentChunkedInput(new ChunkedStream(input))

    ctx.write(response, ctx.voidPromise())
    ctx.write(obj, ctx.voidPromise())
    ctx.write(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{ onResponseComplete(request, _, isKeepAliveWithDebug(response), closeDelay(response)) }
    ctx.flush()
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
    if (includeContentLength(request, response)) {
      HttpUtil.setContentLength(response, length)
      HttpUtil.setTransferEncodingChunked(response, false)
    }

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

    ctx.write(response, ctx.voidPromise())
    ctx.writeAndFlush(obj).onComplete { res: Try[Void] =>
      // Make sure our RandomAccessFile got closed
      raf.close()

      if (res.isFailure) onResponseComplete(request, res, isKeepAliveWithDebug(response), closeDelay(response))
      else ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{ onResponseComplete(request, _, isKeepAliveWithDebug(response), closeDelay(response)) }
    }
  }
  
  /**
   * For sending a Full Http Response
   */
  private def sendFullResponse(request: Request, response: FullHttpResponse)(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendFullResponse")

    // Set the Content-Length since this is a full response which should have a known size
    if (includeContentLength(request, response)) {
      HttpUtil.setContentLength(response, response.content.readableBytes())
      HttpUtil.setTransferEncodingChunked(response, false)
    }
    
    ctx.writeAndFlush(response).onComplete{ onResponseComplete(request, _, isKeepAliveWithDebug(response), closeDelay(response)) }
  }

  /**
   * For sending a chunked http response
   */
  private def sendAsyncResponse(request: Request, response: HttpResponse, head: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendAsyncResponse")

    // We don't know the size in advance since we'll be using: Transfer-Encoding: chunked
    response.headers().remove(HttpHeaderNames.CONTENT_LENGTH)
    HttpUtil.setTransferEncodingChunked(response, true)

    // We writeAndFlush the response headers to get those back to the client as soon as they are ready.
    // Note: The ChannelFuture return from ctx.write(...) doesn't complete unless you eventually call flush.  In this
    //       case we just use ctx.writeAndFlush(...).
    ctx.writeAndFlush(response).onComplete{ onChunkSendComplete(request, _, head, isKeepAliveWithDebug(response), closeDelay(response)) }
  }
  
  /**
   * For triggering the send of a single chunk that is wrapped in a Try
   */
  private def sendChunk(request: Request, chunk: Try[Option[LinkedHttpContent]], isKeepAlive: Boolean, closeDelay: Option[Int])(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendChunk - Try wrapped")
    
    chunk match {
      case Success(optContent) => sendChunk(request, optContent, isKeepAlive, closeDelay)
      case Failure(ex)         => handleFailedChunkFuture(request, ex)
    }
  }
  
  /**
   * For triggering the send of a single chunk
   */
  private def sendChunk(request: Request, chunk: Option[LinkedHttpContent], isKeepAlive: Boolean, closeDelay: Option[Int])(implicit ctx: ChannelHandlerContext): Unit = {
    trace("sendChunk - unwrapped")
    
    chunk match {
      //case Some(content) => require(content.nonEmpty, "Empty Buffer?!"); ctx.writeAndFlush(content).onComplete{ onChunkSendComplete(request, _, content.tail, isKeepAlive) }
      case Some(content) => ctx.writeAndFlush(content).onComplete{ onChunkSendComplete(request, _, content.tail, isKeepAlive, closeDelay) }
      case None          => ctx.writeAndFlush(LastHttpContent.EMPTY_LAST_CONTENT).onComplete{ onResponseComplete(request, _, isKeepAlive, closeDelay) }
    }
  }
  
  private def onChunkSendComplete(request: Request, res: Try[Void], nextChunk: Future[Option[LinkedHttpContent]], isKeepAlive: Boolean, closeDelay: Option[Int])(implicit ctx: ChannelHandlerContext): Unit = {
    trace("onChunkSendComplete")
    
    res match {
      case Success(_)  => nextChunk.onComplete{ sendChunk(request, _, isKeepAlive, closeDelay) }
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
  private def onResponseComplete(request: Request, res: Try[Void], isKeepAlive: Boolean, closeDelay: Option[Int])(implicit ctx: ChannelHandlerContext): Unit = {
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
      case Success(_) =>
        if (readNextRequest) {
          if (logger.isTraceEnabled) logger.trace("ctx.read()")
          ctx.read()
        } else {
          // We want to close the connection
          if (request.isContentFullyRead) {
            closeDelay match {
              case Some(delay) => ctx.channel().eventLoop().schedule(new CloseContextRunnable(ctx), delay, TimeUnit.MILLISECONDS)
              case None => ctx.close()
            }
          } else {
            // If we have sent a response while the client is still trying to send us data let's wait a short time
            // before closing the connection so they have a chance to read our response before seeing a broken pipe
            // on their end.
            //
            // Note: The 5 Second value for the delay is arbitrary
            ctx.channel().eventLoop().schedule(new CloseContextRunnable(ctx), 5, TimeUnit.SECONDS)
          }
        }
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
    
    response.status.code match {
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
  
  private def isKeepAliveWithDebug(response: HttpResponse): Boolean = {
    HttpUtil.isKeepAlive(response) && !response.headers().contains("X-Debug-Force-Connection-Close")
  }

  private def closeDelay(response: HttpResponse): Option[Int] = {
    response.headers().get("X-Debug-Force-Connection-Close-Delay-Millis").toIntOption
  }
}