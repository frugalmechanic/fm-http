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

import java.io.{File, InputStream, RandomAccessFile}
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.handler.codec.http._
import io.netty.util.CharsetUtil
import fm.http.{Cookie, _}
import scala.collection.mutable.Builder

object Response {
  //
  // Helpers for setting Headers on the Response
  //

  /**
   * These are functions that we want applied to the response headers before going back to the user
   */
  private[server] val headerModifications: RequestLocal[Builder[MutableHeaders => Unit, Vector[MutableHeaders => Unit]]] = RequestLocal()

  /**
   * This allows an out-of-band way to modify the response headers that get sent back for a given request
   *
   * These get applied to the headers that come back as part of the Response instance
   *
   * Making this private initially in case we decide we don't want to use arbitrary functions.  The API below
   * (addHeader, setHeader, addCookie) should be enough for our initial use cases.
   */
  private[http] def modifyHeaders(f: MutableHeaders => Unit)(implicit request: Request): Unit = {
    headerModifications.getOrElseUpdate{ Vector.newBuilder[MutableHeaders => Unit] } += f
  }

  /**
   * Add a header to the response
   * @param name The header name
   * @param value The header value
   */
  def addHeader(name: String, value: String)(implicit request: Request): Unit = {
    modifyHeaders{ _.add(name, value) }
  }

  /**
   * Add a header to the response
   * @param name The header name
   * @param value The header value
   */
  def addHeader(name: String, value: Option[String])(implicit request: Request): Unit = {
    // Don't do anything if the value is None
    if (value.isDefined) modifyHeaders{ _.add(name, value.get) }
  }

  /**
   * Set/replace a header to the response
   * @param name The header name
   * @param value The header value
   */
  def setHeader(name: String, value: String)(implicit request: Request): Unit = {
    modifyHeaders{ _.set(name, value) }
  }

  /**
   * Set/replace a header to the response
   * @param name The header name
   * @param value The header value
   */
  def setHeader(name: String, value: Option[String])(implicit request: Request): Unit = {
    modifyHeaders{ _.set(name, value.get) }
  }

  /**
   * Remove a header to the response
   * @param name The header name
   */
  def removeHeader(name: String)(implicit request: Request): Unit = {
    modifyHeaders{ _.remove(name) }
  }

  /**
   * Adds a cookie to the response (via the Set-Cookie header)
   *
   * Note: If a cookie with same name already exists then it is overwritten
   *
   * @param c The Cookie to add
   */
  def addCookie(c: Cookie)(implicit request: Request): Unit = {
    modifyHeaders{ _.addSetCookie(c) }
  }

  //
  // Helpers for creating responses
  //

  /** 200 */
  def Ok(body: String): FullResponse = FullResponse(Status.OK, Headers.empty, Unpooled.copiedBuffer(body, CharsetUtil.UTF_8))
  def Ok(headers: Headers, body: String): FullResponse = FullResponse(Status.OK, headers, Unpooled.copiedBuffer(body, CharsetUtil.UTF_8))
  def Ok(headers: Headers, buf: ByteBuf): FullResponse = FullResponse(Status.OK, headers, buf)
  def Ok(head: LinkedHttpContent): AsyncResponse = AsyncResponse(Status.OK, Headers.empty, head)
  def Ok(headers: Headers, head: LinkedHttpContent): AsyncResponse = AsyncResponse(Status.OK, headers, head)
  def Ok(headers: Headers, file: File): FileResponse = FileResponse(Status.OK, headers, file)
  def Ok(headers: Headers, file: RandomAccessFile): RandomAccessFileResponse = RandomAccessFileResponse(Status.OK, headers, file)
  
  /** 301 */
  def MovedPermanently(location: String): Response = plain(Status.MOVED_PERMANENTLY, location, Headers("Location" -> location))
  
  /** 302 */
  def Found(location: String): Response = plain(Status.FOUND, location, Headers("Location" -> location))
  
  /** 303 */
  def SeeOther(location: String): Response = plain(Status.SEE_OTHER, location, Headers("Location" -> location))
  
  /** 304 */
  def NotModified(headers: Headers = Headers.empty): Response = apply(Status.NOT_MODIFIED, headers)
  
  /** 404 */
  def NotFound(headers: Headers = Headers.empty): Response = apply(Status.NOT_FOUND, headers)
  
  def apply(status: Status, body: String): Response = plain(status, body, Headers.empty)
  def apply(status: Status, body: String, headers: Headers): Response = plain(status, body, headers)
  def apply(status: Status, headers: Headers, body: String): Response = plain(status, body, headers)
  def apply(status: Status): Response = apply(status, Headers.empty)
  def apply(status: Status, headers: Headers): Response = FullResponse(status, headers, Unpooled.EMPTY_BUFFER)
  def apply(status: Status, headers: Headers, arr: Array[Byte]): Response = FullResponse(status, headers, Unpooled.copiedBuffer(arr))
  def apply(status: Status, headers: Headers, arr: Array[Byte], offset: Int, length: Int): Response = FullResponse(status, headers, Unpooled.copiedBuffer(arr, offset, length))
  def apply(status: Status, headers: Headers, buf: ByteBuf): Response = FullResponse(status, headers, buf)

  def plain(status: Status): Response = plain(status, Headers.empty)
  def plain(status: Status, headers: Headers): Response = plain(status, s"${status.code} ${status.msg}", headers)
  def plain(status: Status, headers: Headers, body: String): Response = plain(status, body, headers)
  def plain(status: Status, body: String): Response = FullResponse(status, Headers.empty, Unpooled.copiedBuffer(body, CharsetUtil.UTF_8))
  def plain(status: Status, body: String, headers: Headers): Response = FullResponse(status, headers, Unpooled.copiedBuffer(body, CharsetUtil.UTF_8))

  //
  // The saveAs methods force the browser to prompt the user to Save or Open the file via the Content-Disposition header
  //

  def saveAs(file: File): Response = saveAs(file, file.getName)
  def saveAs(file: File, saveAsName: String): Response = saveAs(file, saveAsName, MimeTypes.forPath(saveAsName).getOrElse{ MimeTypes.BINARY })

  def saveAs(file: File, saveAsName: String, contentType: String): Response = {
    require(file.isFile && file.canRead, "Missing File or no permissions: "+file)
    val raf: RandomAccessFile = new RandomAccessFile(file, "r")
    saveAs(raf, saveAsName, contentType)
  }

  def saveAs(raf: RandomAccessFile, saveAsName: String): Response = saveAs(raf, saveAsName, MimeTypes.forPath(saveAsName).getOrElse{ MimeTypes.BINARY })

  def saveAs(raf: RandomAccessFile, saveAsName: String, contentType: String): Response = {
    val headers: MutableHeaders = MutableHeaders()
    headers.contentType = contentType
    headers.contentDispositionAttachmentFileName = saveAsName
    RandomAccessFileResponse(Status.OK, headers, raf)
  }
}

sealed trait Response extends Message {
  def status: Status
  def headers: Headers
}

/**
 * This represents a full HTTP Response (both headers and complete body)
 */
object FullResponse {
  def apply(status: Status): FullResponse = apply(status, Headers.empty)
  def apply(status: Status, headers: Headers): FullResponse = apply(status, headers, Unpooled.EMPTY_BUFFER)
  def apply(status: Status, headers: Headers, buf: ByteBuf): FullResponse = impl(status, headers, buf)

  private case class impl(status: Status, headers: Headers, buf: ByteBuf) extends FullResponse
}

trait FullResponse extends Response with FullMessage {
  def status: Status
  def headers: Headers
  def buf: ByteBuf

  final def toFullHttpResponse(version: HttpVersion): FullHttpResponse = {
    val r = new DefaultFullHttpResponse(version, status.toHttpResponseStatus, buf)
    r.headers().add(headers.nettyHeaders)
    r
  }
}

/**
 * This represents a chunked HTTP Response with headers and the first chunk along with a pointer to the next chunk
 */
object AsyncResponse {
  def apply(status: Status): AsyncResponse = apply(status, Headers.empty)
  def apply(status: Status, headers: Headers): AsyncResponse = apply(status, headers, LinkedHttpContent(Unpooled.EMPTY_BUFFER))
  def apply(status: Status, headers: Headers, head: LinkedHttpContent): AsyncResponse = impl(status, headers, head)

  private case class impl(status: Status, headers: Headers, head: LinkedHttpContent) extends AsyncResponse
}

trait AsyncResponse extends Response with AsyncMessage {
  def status: Status
  def headers: Headers
  def head: LinkedHttpContent

  final def toHttpResponse(version: HttpVersion): HttpResponse = {
    val r = new DefaultHttpResponse(version, status.toHttpResponseStatus)
    r.headers().add(headers.nettyHeaders)
    r
  }
}

/**
 * This represents a File that we want to send back to the user 
 */
object FileResponse {
  def apply(status: Status, file: File): FileResponse = apply(status, Headers.empty, file)
  def apply(status: Status, headers: Headers, file: File): FileResponse = impl(status, headers, file)

  private case class impl(status: Status, headers: Headers, file: File) extends FileResponse
}

trait FileResponse extends Response with FileMessage {
  def status: Status
  def headers: Headers
  def file: File

  final def toHttpResponse(version: HttpVersion): HttpResponse = {
    val r = new DefaultHttpResponse(version, status.toHttpResponseStatus)
    r.headers().add(headers.nettyHeaders)
    r
  }
}

/**
 * This represents a RandomAccessFile that we want to send back to the user
 */

object RandomAccessFileResponse {
  def apply(status: Status, file: RandomAccessFile): RandomAccessFileResponse = apply(status, Headers.empty, file)
  def apply(status: Status, file: RandomAccessFile, headers: Headers): RandomAccessFileResponse = apply(status, headers, file)
  def apply(status: Status, headers: Headers, file: RandomAccessFile): RandomAccessFileResponse = impl(status, headers, file)

  private case class impl(status: Status, headers: Headers, file: RandomAccessFile) extends RandomAccessFileResponse
}

trait RandomAccessFileResponse extends Response {
  def status: Status
  def headers: Headers
  def file: RandomAccessFile

  final def toHttpResponse(version: HttpVersion): HttpResponse = {
    val r = new DefaultHttpResponse(version, status.toHttpResponseStatus)
    r.headers().add(headers.nettyHeaders)
    r
  }
}


/**
 * This represents an InputStream that we want to send back to the user 
 */

object InputStreamResponse {
  def apply(status: Status, input: InputStream): InputStreamResponse = apply(status, Headers.empty, input, None)
  def apply(status: Status, input: InputStream, length: Option[Long]): InputStreamResponse = apply(status, Headers.empty, input, length)
  def apply(status: Status, headers: Headers, input: InputStream): InputStreamResponse = apply(status, headers, input, None)
  def apply(status: Status, headers: Headers, input: InputStream, length: Option[Long]): InputStreamResponse = impl(status, headers, input, length)

  private case class impl(status: Status, headers: Headers, input: InputStream, length: Option[Long]) extends InputStreamResponse
}

trait InputStreamResponse extends Response with InputStreamMessage {
  def status: Status
  def headers: Headers
  def input: InputStream
  def length: Option[Long]

  final def toHttpResponse(version: HttpVersion): HttpResponse = {
    val r = new DefaultHttpResponse(version, status.toHttpResponseStatus)
    r.headers().add(headers.nettyHeaders)
    r
  }
}
