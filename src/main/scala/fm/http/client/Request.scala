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

import fm.common.URL
import fm.common.Implicits._
import fm.http._
import java.io.File
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.handler.codec.http.{ClientCookieEncoder, DefaultFullHttpRequest, DefaultHttpRequest, FullHttpRequest, HttpHeaders, HttpMessage, HttpRequest, HttpMethod, HttpVersion}
import io.netty.util.CharsetUtil

object Request {
  def Get(url: String, headers: Headers): FullRequest = FullRequest(HttpMethod.GET, URL(url), headers)
  
  def Head(url: String, headers: Headers): FullRequest = FullRequest(HttpMethod.HEAD, URL(url), headers)
  
  def Post(url: String, headers: Headers): FullRequest = FullRequest(HttpMethod.POST, URL(url), headers)
  def Post(url: String, headers: Headers, data: String): FullRequest = FullRequest(HttpMethod.POST, URL(url), headers, Unpooled.copiedBuffer(data, CharsetUtil.UTF_8))
  def Post(url: String, headers: Headers, buf: ByteBuf): FullRequest = FullRequest(HttpMethod.POST, URL(url), headers, buf)
  def Post(url: String, headers: Headers, head: LinkedHttpContent): AsyncRequest = AsyncRequest(HttpMethod.POST, URL(url), headers, head)
  def Post(url: String, headers: Headers, file: File): FileRequest = FileRequest(HttpMethod.POST, URL(url), headers, file)
}

sealed trait Request {
  def method: HttpMethod
  def headers: Headers
  
  /** The complete URL that we are requesting (e.g. http://frugalmechanic.com/foo/bar?param=value) */
  def url: URL
  
  /** This if the URI part of the request:  /foo/bar?param=value */
  def requestURI: String = url.path.getOrElse("/")+url.query.map{ "?"+_ }.getOrElse("")
  
  /** Make a copy of this Request replacing this.headers with newHeaders */
  def withHeaders(newHeaders: Headers): Request
  
  protected def initHeaders[T <: HttpMessage](msg: T): T = {
    msg.headers().add(headers.nettyHeaders)
    msg
  }
  
  override def toString: String = {
    s"${method.name} ${url}\n\n$headers"
  }
}

/**
 * This represents a full HTTP Request (both headers and complete body)
 */
final case class FullRequest(method: HttpMethod, url: URL, headers: Headers = Headers.empty, buf: ByteBuf = Unpooled.EMPTY_BUFFER) extends Request with FullMessage {
  def toFullHttpRequest(version: HttpVersion, uri: String): FullHttpRequest = {
    initHeaders(new DefaultFullHttpRequest(version, method, uri, buf))
  }
  
  def withHeaders(newHeaders: Headers): FullRequest = copy(headers = newHeaders)
}

/**
 * This represents a chunked HTTP Request with headers and the first chunk along with a pointer to the next chunk
 */
final case class AsyncRequest(method: HttpMethod, url: URL, headers: Headers, head: LinkedHttpContent) extends Request with AsyncMessage {
  def toHttpRequest(version: HttpVersion, uri: String): HttpRequest = {
    initHeaders(new DefaultHttpRequest(version, method, uri))
  }
  
  def withHeaders(newHeaders: Headers): AsyncRequest = copy(headers = newHeaders)
}

/**
 * This represents a File that we want to send as the request body
 */
final case class FileRequest(method: HttpMethod, url: URL, headers: Headers, file: File) extends Request with FileMessage {
  def toHttpRequest(version: HttpVersion, uri: String): HttpRequest = {
    initHeaders(new DefaultHttpRequest(version, method, uri))
  }
  
  def withHeaders(newHeaders: Headers): FileRequest = copy(headers = newHeaders)
}