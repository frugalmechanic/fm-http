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

import java.io.{File, InputStream}
import scala.concurrent.Future
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.handler.codec.http._
import io.netty.util.CharsetUtil
import fm.http._

object Response {
  /** 200 */
  def Ok(body: String): FullResponse = FullResponse(Status.OK, Headers.empty, Unpooled.copiedBuffer(body, CharsetUtil.UTF_8))
  def Ok(headers: Headers, body: String): FullResponse = FullResponse(Status.OK, headers, Unpooled.copiedBuffer(body, CharsetUtil.UTF_8))
  def Ok(headers: Headers, buf: ByteBuf): FullResponse = FullResponse(Status.OK, headers, buf)
  def Ok(head: LinkedHttpContent): AsyncResponse = AsyncResponse(Status.OK, Headers.empty, head)
  def Ok(headers: Headers, head: LinkedHttpContent): AsyncResponse = AsyncResponse(Status.OK, headers, head)
  def Ok(headers: Headers, file: File): FileResponse = FileResponse(Status.OK, headers, file)
  
  /** 301 */
  def MovedPermanently(location: String): Response = plain(Status.MOVED_PERMANENTLY, location, Headers("Location" -> location))
  
  /** 302 */
  def Found(location: String): Response = plain(Status.FOUND, location, Headers("Location" -> location))
  
  /** 303 */
  def SeeOther(location: String): Response = plain(Status.SEE_OTHER, location, Headers("Location" -> location))
  
  /** 304 */
  def NotModified(headers: Headers = Headers.empty): Response = apply(Status.NOT_MODIFIED, headers)
  
  def apply(status: Status, body: String): Response = plain(status, body, Headers.empty)
  def apply(status: Status, body: String, headers: Headers): Response = plain(status, body, headers)
  def apply(status: Status, headers: Headers, body: String): Response = plain(status, body, headers)
  def apply(status: Status): Response = apply(status, Headers.empty)
  def apply(status: Status, headers: Headers): Response = FullResponse(status, headers, Unpooled.EMPTY_BUFFER)
  
  def plain(status: Status): Response = plain(status, Headers.empty)
  def plain(status: Status, headers: Headers): Response = plain(status, s"${status.code} ${status.msg}", headers)
  def plain(status: Status, headers: Headers, body: String): Response = plain(status, body, headers)
  def plain(status: Status, body: String, headers: Headers): Response = FullResponse(status, headers, Unpooled.copiedBuffer(body, CharsetUtil.UTF_8))
}

sealed trait Response extends Message {
  def status: Status
  def headers: Headers
}

/**
 * This represents a full HTTP Response (both headers and complete body)
 */
final case class FullResponse(status: Status, headers: Headers = Headers.empty, buf: ByteBuf = Unpooled.EMPTY_BUFFER) extends Response with FullMessage {
  def toFullHttpResponse(version: HttpVersion): FullHttpResponse = {
    val r = new DefaultFullHttpResponse(version, status.toHttpResponseStatus, buf)
    r.headers().add(headers.nettyHeaders)
    r
  }
}

/**
 * This represents a chunked HTTP Response with headers and the first chunk along with a pointer to the next chunk
 */
final case class AsyncResponse(status: Status, headers: Headers, head: LinkedHttpContent) extends Response with AsyncMessage {
  def toHttpResponse(version: HttpVersion): HttpResponse = {
    val r = new DefaultHttpResponse(version, status.toHttpResponseStatus)
    r.headers().add(headers.nettyHeaders)
    r
  }
}

/**
 * This represents a File that we want to send back to the user 
 */
final case class FileResponse(status: Status, headers: Headers, file: File) extends Response with FileMessage {
  def toHttpResponse(version: HttpVersion): HttpResponse = {
    val r = new DefaultHttpResponse(version, status.toHttpResponseStatus)
    r.headers().add(headers.nettyHeaders)
    r
  }
}

/**
 * This represents an InputStream that we want to send back to the user 
 */
final case class InputStreamResponse(status: Status, headers: Headers, input: InputStream, length: Option[Long]) extends Response with InputStreamMessage {
  def toHttpResponse(version: HttpVersion): HttpResponse = {
    val r = new DefaultHttpResponse(version, status.toHttpResponseStatus)
    r.headers().add(headers.nettyHeaders)
    r
  }
}
