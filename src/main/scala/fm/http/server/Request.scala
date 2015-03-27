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

import fm.common.{IP, Logging, QueryParams}
import fm.http._
import java.io.Closeable
import io.netty.handler.codec.http.{HttpHeaders, HttpMethod, HttpVersion}
import io.netty.handler.codec.http.{DefaultHttpContent, DefaultHttpRequest, HttpRequest, LastHttpContent}
import io.netty.handler.codec.http.multipart.{HttpPostRequestDecoder, InterfaceHttpData}
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future, Promise}

object Request {
  def apply(remoteIp: IP, request: HttpRequest, content: LinkedHttpContentReader)(implicit execution: ExecutionContext): Request = new Request(remoteIp, request, content)
  
  private def expectBodyContent(request: HttpRequest): Boolean = {
    import HttpMethod.{POST, PUT, PATCH}
    
    val method: HttpMethod = request.getMethod()
    val validMethod: Boolean = method == POST || method == PUT || method == PATCH
    val hasContentLength: Boolean = HttpHeaders.getContentLength(request, -1) > 0
    val isChunked: Boolean = HttpHeaders.isTransferEncodingChunked(request)
    
    validMethod && (hasContentLength || isChunked)
  }
}

final class Request (
  val remoteIp: IP, // The Remote IP making the request (possibly extracted from FM-Remote-IP or X-Forwarded-For)
  request: HttpRequest,
  content: LinkedHttpContentReader
)(implicit execution: ExecutionContext) extends Logging with Closeable {
  
  private[this] val completedPromise: Promise[Unit] = Promise()
  
  /**
   * This future is completed when the request has been fully processed
   */
  def completed: Future[Unit] = completedPromise.future
  
  val version: HttpVersion = request.getProtocolVersion()
  
  val method: HttpMethod = request.getMethod()
  
  val uri: String = request.getUri()
  
  val headers: ImmutableHeaders = ImmutableHeaders(request.headers)
  
  /**
   * The Content-Length of the request body (if known)
   */
  def contentLength: Option[Long] = Option(HttpHeaders.getContentLength(request, -1)).filter{ _ >= 0 }
  
  /**
   * The path that was requested (no query params, e.g. /path)
   */
  val path: String = {
    // Strip off anything after the ?
    val qIdx: Int = uri.indexOf('?')
    val tmp: String = if (qIdx != -1) uri.substring(0, qIdx) else uri

    // Strip off anything after the #
    val hIdx: Int = uri.indexOf('#')
    if (hIdx != -1) tmp.substring(0, hIdx) else tmp
  }
  
  /**
   * The parsed query params
   */
  val params: QueryParams = if (uri.contains('?')) QueryParams(uri) else QueryParams.empty
  
  /**
   * The raw value of the Host headers which may contain a port number:  frugalmechanic.com:8080
   */
  val hostWithPort: Option[String] = headers.host
  
  /**
   * The value of the Host header WITHOUT the port number
   */
  val host: Option[String] = headers.hostWithoutPort

  private def requireEmptyContent(): Unit = {
    content.foldLeft(false){ (isSet, buf) =>
      if (isSet) logger.error("Expected EmptyContent for request: "+this)
      true
    }
  }
  
  private[this] val postDecoder: Option[HttpPostRequestDecoder] = if (Request.expectBodyContent(request)) {
    Some(new HttpPostRequestDecoder(request))
  } else {
    requireEmptyContent()
    None
  }
  
  /**
   * The contents of the POST/PUT/PATCH body.  Only access this value if you want to initiate receiving the data
   */
  lazy val postBody: Future[PostBody] = postDecoder match {
    case Some(decoder) => content.foldLeft(decoder){ (decoder, buf) => decoder.offer(new DefaultHttpContent(buf)); decoder }.map{ _.offer(LastHttpContent.EMPTY_LAST_CONTENT) }.map{ decoder => PostBody.fromNetty(decoder.getBodyHttpDatas().asScala.toVector) }
    case None => Future.successful(PostBody.empty)
  }
  
  def isContentFullyRead: Boolean = if (Request.expectBodyContent(request)) content.isFullyRead else true
  
  def close(): Unit = close(null)
  
  def close(t: Throwable): Unit = {
    if (null == t) completedPromise.trySuccess(()) else completedPromise.tryFailure(t)
    
    postDecoder.foreach { decoder: HttpPostRequestDecoder =>
      try {
        decoder.destroy()
      } catch {
        case _: IllegalStateException => // This will get thrown if destroy() has already been called
      }
    }
  }
  
  override protected def finalize(): Unit = close()
}
