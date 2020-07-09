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
import io.netty.handler.codec.http.{DefaultFullHttpRequest, DefaultHttpRequest, FullHttpRequest, HttpMessage, HttpMethod, HttpRequest, HttpVersion}
import io.netty.util.CharsetUtil
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Request {
  def Get(url: String, headers: Headers): FullRequest = FullRequest(HttpMethod.GET, URL(url), headers)
  
  def Head(url: String, headers: Headers): FullRequest = FullRequest(HttpMethod.HEAD, URL(url), headers)
  
  def Post(url: String, headers: Headers): FullRequest = FullRequest(HttpMethod.POST, URL(url), headers)
  def Post(url: String, headers: Headers, data: String): FullRequest = FullRequest(HttpMethod.POST, URL(url), headers, Unpooled.copiedBuffer(data, CharsetUtil.UTF_8))
  def Post(url: String, headers: Headers, data: Array[Byte]): FullRequest = FullRequest(HttpMethod.POST, URL(url), headers, Unpooled.copiedBuffer(data))
  def Post(url: String, headers: Headers, buf: ByteBuf): FullRequest = FullRequest(HttpMethod.POST, URL(url), headers, buf)
  def Post(url: String, headers: Headers, head: LinkedHttpContent): AsyncRequest = AsyncRequest(HttpMethod.POST, URL(url), headers, Future.successful(Option(head)))
  def Post(url: String, headers: Headers, head: Future[Option[LinkedHttpContent]]): AsyncRequest = AsyncRequest(HttpMethod.POST, URL(url), headers, head)
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

  private[client] def isSendable: Boolean
  private[client] def refCnt(): Int
  private[client] def retain(): this.type
  private[client] def release(decrement: Int): Boolean
  private[client] def readerIndex(): Int
  private[client] def readerIndex(idx: Int): Unit
}

/**
 * This represents a full HTTP Request (both headers and complete body)
 */
final case class FullRequest(method: HttpMethod, url: URL, headers: Headers = Headers.empty, buf: ByteBuf = Unpooled.EMPTY_BUFFER) extends Request with FullMessage {
  def toFullHttpRequest(version: HttpVersion, uri: String): FullHttpRequest = {
    initHeaders(new DefaultFullHttpRequest(version, method, uri, buf))
  }

  override def withHeaders(newHeaders: Headers): FullRequest = copy(headers = newHeaders)

  override private[client] def isSendable: Boolean = buf.refCnt() > 0 // Sendable as long as the buf hasn't been fully released

  override private[client] def refCnt(): Int = buf.refCnt()

  override private[client] def retain(): this.type = {
    buf.retain()
    this
  }

  override private[client] def release(decrement: Int): Boolean = buf.release(decrement)
  override private[client] def readerIndex(): Int = buf.readerIndex()
  override private[client] def readerIndex(idx: Int): Unit = buf.readerIndex(idx)
}

/**
 * This represents a chunked HTTP Request with headers and the first chunk along with a pointer to the next chunk
 */
final case class AsyncRequest(method: HttpMethod, url: URL, headers: Headers, head: Future[Option[LinkedHttpContent]]) extends Request with AsyncMessage {
  def toHttpRequest(version: HttpVersion, uri: String): HttpRequest = {
    initHeaders(new DefaultHttpRequest(version, method, uri))
  }

  override def withHeaders(newHeaders: Headers): AsyncRequest = copy(headers = newHeaders)

  override private[client] def isSendable: Boolean = {
    if (!head.isCompleted) return true // Future isn't even completed so it should be sendable

    head.value match {
      case None => true // Not yet completed, should be okay to send
      case Some(t: Try[Option[LinkedHttpContent]]) =>
        t match {
          case Failure(_) => true // Should attempt send and follow normal failure code path
          case Success(None) => true  // Empty content, is okay to send
          case Success(Some(_: LinkedHttpContent)) => false // For now this case is false.  More work would be required to properly handle this (if it is even possible)
        }
    }
  }

  override private[client] def refCnt(): Int = 1 // nop
  override private[client] def retain(): this.type = this // nop
  override private[client] def release(decrement: Int): Boolean = true // nop
  override private[client] def readerIndex(): Int = 0 // nop
  override private[client] def readerIndex(idx: Int): Unit = { } // nop
}

/**
 * This represents a File that we want to send as the request body
 */
final case class FileRequest(method: HttpMethod, url: URL, headers: Headers, file: File) extends Request with FileMessage {
  def toHttpRequest(version: HttpVersion, uri: String): HttpRequest = {
    initHeaders(new DefaultHttpRequest(version, method, uri))
  }

  override def withHeaders(newHeaders: Headers): FileRequest = copy(headers = newHeaders)

  override private[client] def isSendable: Boolean = true // Always sendable
  override private[client] def refCnt(): Int = 1 // nop
  override private[client] def retain(): this.type = this // nop
  override private[client] def release(decrement: Int): Boolean = false // nop
  override private[client] def readerIndex(): Int = 0 // nop
  override private[client] def readerIndex(idx: Int): Unit = { } // nop
}