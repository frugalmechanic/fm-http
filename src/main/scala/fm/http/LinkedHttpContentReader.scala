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

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{DefaultFullHttpResponse, FullHttpResponse, HttpResponseStatus, HttpVersion}
import io.netty.util.CharsetUtil
import java.io.{Closeable, File, FileOutputStream}
import java.nio.charset.Charset
import java.util.concurrent.atomic.AtomicBoolean
import fm.common.Implicits._

object LinkedHttpContentReader {
  def apply(need100Continue: Boolean, head: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext, executor: ExecutionContext): LinkedHttpContentReader = new LinkedHttpContentReader(need100Continue, head)
  
  private val CONTINUE: FullHttpResponse = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE, Unpooled.EMPTY_BUFFER)  
}

final class LinkedHttpContentReader(is100ContinueExpected: Boolean, head: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext, executor: ExecutionContext) extends Closeable {
  import LinkedHttpContentReader.CONTINUE

  private[this] var foldLeftCalled: AtomicBoolean = new AtomicBoolean(false)
  @volatile private[this] var current: Future[Option[LinkedHttpContent]] = head

  /**
   * Have we started reading this request content?
   */
  def hasStartedReading: Boolean = foldLeftCalled.get()
  
  /**
   * Has the content been fully read successfully
   */
  def isFullyRead: Boolean = future.isSuccess
  
  private[this] val completedPromise: Promise[Unit] = Promise()
  
  /**
   * This will be completed when the body is fully read (or an exception is thrown)
   */
  def future: Future[Unit] = completedPromise.future
  
  /**
   * Read the response body into a string
   */
  def readToString(maxLength: Long = Long.MaxValue, encoding: Charset = CharsetUtil.ISO_8859_1): Future[String] = {
    foldLeft(new StringBuilder){ (sb: StringBuilder, buf: ByteBuf) =>
      sb.append(buf.toString(encoding))
      require(sb.length <= maxLength, s"Body exceeds maxLength.  Body Length (so far): ${sb.length}  Specified Max Length: $maxLength")
      sb
    }.map{ _.toString }
  }
  
  /**
   * Write this body to a file
   */
  def writeToFile(file: File): Future[Unit] = {
    val os: FileOutputStream = new FileOutputStream(file)
    val f: Future[Unit] = foldLeft(os){ (os, buf) =>
      buf.readBytes(os, buf.readableBytes())
      os
    }.map{ _ => Unit }
    
    f.onComplete{ case _ => os.close() }
    
    f
  }
  
  /**
   * This is an asynchronous foreach where 'f' is called for each ByteBuf as the data is available.
   * @returns A future is returned that is completed when 'f' has been called for each chunk.
   */
  def foreach[U](f: ByteBuf => U): Future[Unit] = foldLeft[Unit](){ (unit, buf) => f(buf) }.map{ _ => Unit }
  
  /**
   * This is an asynchronous foldLeft where op is called only when the chunks are ready.
   * @returns The Result is returned as a Future
   */
  def foldLeft[B](z: B)(op: (B, ByteBuf) => B): Future[B] = synchronized {
    require(foldLeftCalled.compareAndSet(false, true), "foldLeft already called!")
    require(null != current, "current == null which means the data was already read!")
    
    val p: Promise[B] = Promise()
    
    if (is100ContinueExpected) ctx.writeAndFlush(CONTINUE)
    
    foldLeft0(current)(z, op, p)
    
    // Ditch the current reference as soon as we start reading so we don't end up
    // referencing the entire POST body in memory
    current = null
    
    completedPromise.completeWith(p.future.map{ _ => Unit })
    
    p.future
  }
  
  private def foldLeft0[B](chunk: Future[Option[LinkedHttpContent]])(z: B, op: (B, ByteBuf) => B, p: Promise[B]): Unit = {
    ctx.read()
    
    chunk.onComplete{ t: Try[Option[LinkedHttpContent]] =>
      t match {
        case Failure(ex) =>             p.failure(ex)
        case Success(opt) => opt match {
          case None =>                  p.success(z)
          case Some(linkedContent) =>
            // Wrap the op in a try in case it throws an exception
            val t = Try{ op(z, linkedContent.content()) }
            linkedContent.release() // Release the ByteBuf reference since we've now read the data
            t match {
              case Failure(ex)   => p.failure(ex); ctx.close()
              case Success(newZ) => foldLeft0(linkedContent.tail)(newZ, op, p)
            }
        }
      }
    }
  }
  
  def close(): Unit = try {
    if (!foldLeftCalled.get()) { 
      foldLeft(){ (_, buf) => Unit }
    }
  } catch {
    case ex: Exception => // ok
  }
  
  override protected def finalize(): Unit = close()
  
}