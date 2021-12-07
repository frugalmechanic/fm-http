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

import fm.common.{IP, Logging, QueryParams, UUID}
import fm.common.Implicits._
import fm.http._
import io.netty.buffer.ByteBuf
import java.io.Closeable
import java.util.IdentityHashMap
import io.netty.handler.codec.http._
import io.netty.handler.codec.http.multipart.HttpPostRequestDecoder
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future, Promise}

object Request {
  def apply(
    remoteIp: IP,
    request: HttpRequest,
    content: LinkedHttpContentReader
  )(implicit execution: ExecutionContext): Request = {
    new Request(remoteIp, request, content)
  }

  def dummy(remoteIp: IP, method: HttpMethod, uri: String)(implicit execution: ExecutionContext): Request = {
    apply(remoteIp, new DefaultHttpRequest(HttpVersion.HTTP_1_1, method, uri), LinkedHttpContentReader.empty)
  }

  def empty(implicit execution: ExecutionContext): Request = dummy(IP(0), HttpMethod.GET, "/")

  private def expectBodyContent(request: HttpRequest): Boolean = {
    import HttpMethod.{POST, PUT, PATCH}
    
    val method: HttpMethod = request.method()
    val validMethod: Boolean = method === POST || method === PUT || method === PATCH
    val hasContentLength: Boolean = HttpUtil.getContentLength(request, -1) > 0
    val isChunked: Boolean = HttpUtil.isTransferEncodingChunked(request)
    
    validMethod && (hasContentLength || isChunked)
  }
}

final class Request (
  val remoteIp: IP, // The Remote IP making the request (possibly extracted from X-Forwarded-For)
  request: HttpRequest,
  val content: LinkedHttpContentReader
)(implicit execution: ExecutionContext) extends Logging with Closeable {
  /**
   * A fm.common.UUID that can be used to uniquely identify this request
   */
  val id: UUID = UUID()

  /**
   * The time this Request started
   */
  val startTimeMillis: Long = System.currentTimeMillis()
  
  private[this] val completedPromise: Promise[Unit] = Promise()

  // This is ONLY accessed by the RequestLocal class (synchronization is performed there)
  // This is initialized lazily to avoid creating the IdentityHashMap if we never store anything in it.
  private[server] var requestLocalMap: IdentityHashMap[RequestLocal[_],AnyRef] = null
  
  /**
   * This future is completed when the request has been fully processed
   */
  def completed: Future[Unit] = completedPromise.future
  
  val version: HttpVersion = request.protocolVersion()
  
  val method: HttpMethod = request.method()
  
  val uri: String = request.uri()
  
  val headers: ImmutableHeaders = ImmutableHeaders(request.headers)
  
  /**
   * The Content-Length of the request body (if known)
   */
  def contentLength: Option[Long] = Option(HttpUtil.getContentLength(request, -1)).filter{ _ >= 0 }.map{ _.toLong }
  
  /**
   * The path that was requested (no query params, e.g. /path)
   */
  val path: String = {
    // Strip off anything after the ?
    val qIdx: Int = uri.indexOf('?')
    val tmp: String = if (qIdx != -1) uri.substring(0, qIdx) else uri

    // Strip off anything after the #
    val hIdx: Int = tmp.indexOf('#')
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
    // If the user has already triggered a read method then
    // ignore this check since it will trigger an exception.
    if (content.hasStartedReading) return
    
    content.foldLeft(false){ (isSet, buf) =>
      if (isSet) logger.error("Expected EmptyContent for request: "+this)
      true
    }
  }
  
  // TODO: figure out a better strategy for this and the postBody val.  Note: This is lazy
  // to avoid triggering the requireEmptyContent() when the content-length is 0
  private[this] lazy val postDecoder: Option[HttpPostRequestDecoder] = if (Request.expectBodyContent(request)) {
    Some(new HttpPostRequestDecoder(request))
  } else {
    requireEmptyContent()
    None
  }
  
  /**
   * The contents of the POST/PUT/PATCH body.  Only access this value if you want to initiate receiving the data
   */
  lazy val postBody: Future[PostBody] = postDecoder match {
    case None => Future.successful(PostBody.empty)
    case Some(decoder) =>
      content.foldLeft(decoder){ (decoder: HttpPostRequestDecoder, buf: ByteBuf) =>
        decoder.offer(new DefaultHttpContent(buf))
        decoder
      }.map{ decoder: HttpPostRequestDecoder =>
        decoder.offer(LastHttpContent.EMPTY_LAST_CONTENT)
        PostBody.fromNetty(decoder.getBodyHttpDatas().asScala.toVector)
      }
  }

  def isOPTIONS: Boolean = method === HttpMethod.OPTIONS
  def isGET: Boolean = method === HttpMethod.GET
  def isHEAD: Boolean = method === HttpMethod.HEAD
  def isPOST: Boolean = method === HttpMethod.POST
  def isPUT: Boolean = method === HttpMethod.PUT
  def isPATCH: Boolean = method === HttpMethod.PATCH
  def isDELETE: Boolean = method === HttpMethod.DELETE
  def isTRACE: Boolean = method === HttpMethod.TRACE
  def isCONNECT: Boolean = method === HttpMethod.CONNECT
 
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
}
