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

import io.netty.handler.codec.http.{HttpMessage, HttpResponse, HttpResponseDecoder, HttpUtil, HttpVersion}
import java.io.Closeable
import java.nio.charset.{Charset, IllegalCharsetNameException}
import scala.concurrent.{ExecutionContext, Future, Promise}
import fm.common.Logging
import fm.common.Implicits._
import fm.http._
import scala.util.matching.Regex

object Response {
  def apply(response: HttpResponse, content: LinkedHttpContentReader)(implicit execution: ExecutionContext): AsyncResponse = new AsyncResponse(response, content)

  val CharsetRegex = """(?i)^\s*?.*?\s*?charset\s*?=\s*?(.*?)$""".r

  // Semi-hack to access this protected helper
  private object Decoder extends HttpResponseDecoder {
    override def isContentAlwaysEmpty(msg: HttpMessage): Boolean = super.isContentAlwaysEmpty(msg)
  }

  private[client] def isContentAlwaysEmpty(msg: HttpMessage): Boolean = Decoder.isContentAlwaysEmpty(msg)
}

sealed abstract class Response(response: HttpResponse) extends Closeable {
  val status: Status = Status(response.status())
  
  val version: HttpVersion = response.protocolVersion()
  
  val headers: ImmutableHeaders = ImmutableHeaders(response.headers)

  def isContentAlwaysEmpty: Boolean = Response.isContentAlwaysEmpty(response)

  /**
   * Any Set-Cookie Headers are decoded into Cookies
   */
  val cookies: Vector[Cookie] = headers.setCookies
  
  /**
   * The Content-Length of the request body (if known)
   */
  def contentLength: Option[Long] = Option(HttpUtil.getContentLength(response, -1)).filter{ _ >= 0 }.map{ _.toLong }
  
  override def toString: String = {
    s"${version.text} ${status.code} ${status.msg}\n\n$headers"
  }

  protected def detectCharset(defaultCharset: Charset): Charset = {
    import Response.CharsetRegex

    response.headers().get("Content-Type").toBlankOption.flatMap { (contentType: String) =>
      val contentTypeCharset: Option[Charset] = for {
        charsetMatch: Regex.Match <- CharsetRegex.findFirstMatchIn(contentType)
        charset: String = charsetMatch.group(1)
        if charsetIsSupported(charset)
      } yield Charset.forName(charset)

      contentTypeCharset orElse MimeTypes.mimeTypeToCharset.get(contentType)
    }
  }.getOrElse(defaultCharset)

  private def charsetIsSupported(charset: String): Boolean = {
    // Charset.isSupported will throw an IllegalArgumentException charset is null.  This explicitly handles that case.
    if (charset.isNullOrBlank) return false

    try {
      // Note: This will throw IllegalCharsetNameException for stuff like "\"UTF-8\"" or "-UTF-8" which we
      //       need to translate to false instead of letting an exception be thrown
      Charset.isSupported(charset)
    } catch {
      case _: IllegalCharsetNameException => false
    }
  }
}

/**
 * Represents a response where we have the FULL body (as a string)
 */
final class FullStringResponse(response: HttpResponse, val body: String) extends Response(response) {
  def close(): Unit = { }
}

/**
 * Represents a response where we have the FULL body (as an array of bytes)
 */
final class FullResponse(response: HttpResponse, val body: Array[Byte]) extends Response(response) {
  def close(): Unit = { }
}

object AsyncResponse {
  // https://tools.ietf.org/html/rfc7230#section-3.3.3
  //   7.  Otherwise, this is a response message without a declared message
  //       body length, so the message body length is determined by the
  //       number of octets received prior to the server closing the
  //       connection.
  private def expectBodyContent(response: HttpResponse): Boolean = {
    if (Response.isContentAlwaysEmpty(response)) return false

    val contentLength: Long = HttpUtil.getContentLength(response, -1L)
    val hasRequiredContentLength: Boolean = contentLength > 0L || (!HttpUtil.isKeepAlive(response) && contentLength === -1L)

    val isChunked: Boolean = HttpUtil.isTransferEncodingChunked(response)

    hasRequiredContentLength || isChunked
  }
}

/**
 * Represents a Response where the body of the response can be read asynchronously (which means it can be much larger than available heap)
 */
final class AsyncResponse (response: HttpResponse, content: LinkedHttpContentReader)(implicit execution: ExecutionContext) extends Response(response) with Logging {
  
  /**
   * Should this response contain a non-empty body?
   */
  val hasBody: Boolean = AsyncResponse.expectBodyContent(response)
  
  /**
   * If there is no body then the promise starts completed
   */
  private[this] val completedPromise: Promise[Unit] = if (!hasBody) Promise.successful(()) else Promise()
  
  /**
   * This future is completed when the request has been fully processed
   */
  def completed: Future[Unit] = completedPromise.future

  if (!hasBody) requireEmptyContent()
  
  private def requireEmptyContent(): Unit = {
    content.foldLeft(false){ (isSet, buf) =>
      if (isSet) logger.error("Expected EmptyContent for response: "+this)
      true
    }
  }
  
  val body: Option[LinkedHttpContentReader] = if (hasBody) Some(content) else None

  def toFullStringResponse(maxLength: Long = Long.MaxValue, defaultCharset: Charset = null): Future[FullStringResponse] = body match {
    case None         => Future.successful(new FullStringResponse(response, ""))
    case Some(reader) => reader.readToString(maxLength, detectCharset(defaultCharset)).map{ new FullStringResponse(response, _) }
  }

  def readBodyToString(maxLength: Long = Long.MaxValue, defaultCharset: Charset = null): Future[String] = {
    val f: Future[String] = body.map{ _.readToString(maxLength, detectCharset(defaultCharset)) }.getOrElse{ Future.successful("") }
    f.onComplete{ case _ => close() }
    f
  }

  def toFullResponse(maxLength: Long = Long.MaxValue): Future[FullResponse] = body match {
    case None         => Future.successful(new FullResponse(response, Array.empty[Byte]))
    case Some(reader) => reader.readToByteArray(maxLength).map{ new FullResponse(response, _) }
  }

  
  def readBodyToByteArray(maxLength: Long = Long.MaxValue): Future[Array[Byte]] = {
    val f = body.map{ _.readToByteArray(maxLength) }.getOrElse{ Future.successful(Array.empty[Byte]) }
    f.onComplete{ case _ => close() }
    f
  }
  
  def isContentFullyRead: Boolean = if (hasBody) content.isFullyRead else true
  
  def close(): Unit = close(null)
  
  def close(t: Throwable): Unit = {
    body.foreach{ _.close() }
    if (null == t) completedPromise.trySuccess(()) else completedPromise.tryFailure(t)
  }
}
