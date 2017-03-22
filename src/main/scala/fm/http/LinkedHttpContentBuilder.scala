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

import fm.common.Implicits._
import java.lang.ref.WeakReference
import java.io.Closeable
import io.netty.buffer.ByteBuf
import io.netty.handler.codec.http.{HttpContent, HttpObject, LastHttpContent}
import scala.concurrent.{Future, Promise}

object LinkedHttpContentBuilder {
  val empty: LinkedHttpContentBuilder = {
    // This is a little hacky since we are touching internal state
    val tmp = LinkedHttpContentBuilder()
    tmp.done = true
    tmp.first = false
    tmp.nextChunk.success(None)
    tmp
  }
  
  def isEmptyLastContent(obj: HttpObject): Boolean = obj match {
    case content: LastHttpContent => isEmptyLastContent(content)
    case _ => false
  }
  
  private def isEmptyLastContent(content: HttpContent): Boolean = isEmpty(content) && content.isInstanceOf[LastHttpContent]
  
  private def isEmpty(content: HttpContent): Boolean = content.content().readableBytes() === 0
  private def nonEmpty(content: HttpContent): Boolean = !isEmpty(content)
}

final case class LinkedHttpContentBuilder() extends Closeable {
  import LinkedHttpContentBuilder.nonEmpty
  
  @volatile private var done: Boolean = false
  @volatile private var first: Boolean = true
  @volatile private var nextChunk: Promise[Option[LinkedHttpContent]] = Promise()
  
  /**
   * This is a WeakReference (although since we clear it ourselves it probably doesn't need to be) because we 
   * don't want to hold onto a reference to the head of this LinkedHttpContent since for a large body that
   * could eat up a lot of memory. 
   */
  private[this] val head: WeakReference[Future[Option[LinkedHttpContent]]] = new WeakReference(nextChunk.future)
  
  /**
   * This is only valid while we are still on the first chunk (i.e. done() and +=() hasn't been called)
   */
  def future: Future[Option[LinkedHttpContent]] = Option(head.get).getOrElse{ throw new NoSuchElementException("INVALID USAGE -- You must call 'future' BEFORE calling 'done()' or '+=(...)'") }
  
  def isDone: Boolean = done
  
  def close(): Unit = +=(None)
  
  def +=(next: HttpContent): this.type = {

    if (nonEmpty(next)) {
      val buf: ByteBuf = next.content()
      buf.retain()
      +=(buf)
    }
    
    if (next.isInstanceOf[LastHttpContent]) close()
    
    this
  }
  
  def +=(next: ByteBuf): this.type = +=(Option(next))
  
  def +=(next: Option[ByteBuf]): this.type = synchronized {
    // close() can be called multiple times
    if (done && next.isEmpty) return this
    
    require(!done, "Already Done!")
    
    if (first) {
      // Clear the weak reference
      head.clear()
      first = false
    }
    
    next match {
      case None => 
        nextChunk.success(None)
        nextChunk = null
        done = true
        
      case Some(buf) =>
        val current = nextChunk
        nextChunk = Promise()
        current.success(Some(LinkedHttpContent(buf, nextChunk.future)))
    }
    
    this
  }
  
  def +=(cause: Throwable): this.type = synchronized {
    if (done) return this
    
    nextChunk.failure(cause)
    nextChunk = null
    done = true
    this
  }
}