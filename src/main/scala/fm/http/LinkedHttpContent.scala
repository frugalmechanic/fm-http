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
package fm.http

import io.netty.buffer.ByteBuf
import io.netty.handler.codec.http.DefaultHttpContent
import scala.concurrent.Future
import scala.util.Try

object LinkedHttpContent {
  val EOF: Future[Option[LinkedHttpContent]] = Future.successful(None)
  
  def apply(data: ByteBuf): LinkedHttpContent = apply(data, EOF)
  
  /**
   * Note: next is NOT lazily evaluated using this apply method because it can lead to weird things
   *       happening if the caller isn't aware it's a by-name parameter.
   */
  def apply(data: ByteBuf, next: Future[Option[LinkedHttpContent]]): LinkedHttpContent = new LinkedHttpContent(data, next)
  
  /**
   * Same as apply(data, next) except that next is lazily evaluated
   */
  def async(data: ByteBuf, next: => Future[Option[LinkedHttpContent]]): LinkedHttpContent = new LinkedHttpContent(data, next)
}

/**
 * This represents asynchronous chunked response content where chunks are chained together with futures that are
 * evaluated on-demand to allow for streaming responses that can be memory efficient (i.e. we don't have to evaluate all chunks before
 * we are ready to send them)
 */
final class LinkedHttpContent(data: ByteBuf, next: => Future[Option[LinkedHttpContent]]) extends DefaultHttpContent(data) {
  def isEmpty: Boolean = !nonEmpty
  def nonEmpty: Boolean = data.readableBytes() > 0
  
  /**
   * This is the next LinkedHttpContent which is lazily evaluated
   */
  lazy val tail: Future[Option[LinkedHttpContent]] = next

  /**
   * This is a non-blocking call to check if the next LinkedHttpContent is None
   * 
   * Returns:
   *  true - Yes this is the last content chunk
   *  false - We don't know if this is the last content chunk
   */
  def isEndOfContent: Boolean = {
    tail.value.exists { t: Try[Option[LinkedHttpContent]] =>
      // The next Option[LinkedHttpContent] should be None if this is the last content
      t.map{ _.isEmpty }.getOrElse(false)
    }
  }
}