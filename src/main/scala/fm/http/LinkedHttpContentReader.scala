/*
 * Copyright 2015 Frugal Mechanic (http://frugalmechanic.com)
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

import fm.common.{IOUtils, Logging}
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{DefaultFullHttpResponse, FullHttpResponse, HttpResponseStatus, HttpVersion}
import io.netty.util.CharsetUtil
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, Closeable, File, FileOutputStream}
import java.nio.charset.Charset
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object LinkedHttpContentReader {
  def apply(need100Continue: Boolean, head: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext, executor: ExecutionContext): LinkedHttpContentReader = new LinkedHttpContentReader(need100Continue, head)

  def empty(implicit executor: ExecutionContext): LinkedHttpContentReader = {
    val ctx: ChannelHandlerContext = null
    LinkedHttpContentReader(false, Future.successful(None))(ctx, executor)
  }

  private val CONTINUE: FullHttpResponse = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE, Unpooled.EMPTY_BUFFER)  
}

final class LinkedHttpContentReader(is100ContinueExpected: Boolean, head: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext, executor: ExecutionContext) extends Closeable with Logging {
  import LinkedHttpContentReader.CONTINUE

  private[this] val foldLeftCalled: AtomicBoolean = new AtomicBoolean(false)
  private[this] val hasBeenFullyRead: AtomicBoolean = new AtomicBoolean(false)
  @volatile private[this] var current: Future[Option[LinkedHttpContent]] = head

  /**
   * Have we started reading this request content?
   */
  def hasStartedReading: Boolean = foldLeftCalled.get()

  /**
   * Has the content been fully read successfully
   */
  def isFullyRead: Boolean = hasBeenFullyRead.get()

  // This method was originally implemented in terms of the future being successfully completed but
  // the completion of the future could be slightly delayed and therefore not accurate when we call
  // this method from the NettyHttpServerPipelineHandler.prepareResponse method to determine if we
  // need to warn about the request body not being fully read.  So now we use an AtomicBoolean set
  // by the foldLeft0 method.
  //
  // Note: this requires "import fm.common.Implicits._"
  //def isFullyRead: Boolean = future.isSuccess
  
  private[this] val completedPromise: Promise[Unit] = Promise()
  
  /**
   * This will be completed when the body is fully read (or an exception is thrown)
   */
  def future: Future[Unit] = completedPromise.future
  
  /**
   * Read the response body into an Array[Byte]
   */
  def readToByteArray(): Future[Array[Byte]] = readToByteArray(Long.MaxValue)

  def readToByteArray(maxLength: Long): Future[Array[Byte]] = {
    foldLeft(new ByteArrayOutputStream){ (out: ByteArrayOutputStream, buf: ByteBuf) =>
      buf.readBytes(out, buf.readableBytes())
      require(out.size() <= maxLength, s"Body exceeds maxLength.  Body Length (so far): ${out.size}  Specified Max Length: $maxLength")
      out
    }.map{ _.toByteArray }
  }
  
  /**
   * Read the response body into a string
   */

  def readToString(maxLength: Long): Future[String] = readToString(maxLength, CharsetUtil.ISO_8859_1)
  def readToString(encoding: Charset): Future[String] = readToString(Long.MaxValue, CharsetUtil.ISO_8859_1)

  def readToString(maxLength: Long, encoding: Charset): Future[String] = {
    if (null == encoding) readToStringWithDetectedCharset(maxLength) else readToStringWithCharset(encoding, maxLength)
  }

  def readToStringWithDetectedCharset(maxLength: Long): Future[String] = readToStringWithDetectedCharset(maxLength, CharsetUtil.ISO_8859_1)
  def readToStringWithDetectedCharset(defaultEncoding: Charset): Future[String] = readToStringWithDetectedCharset(Long.MaxValue, CharsetUtil.ISO_8859_1)

  def readToStringWithDetectedCharset(maxLength: Long, defaultEncoding: Charset): Future[String] = {
    readToByteArray(maxLength).map{ bytes: Array[Byte] =>
      val charset: Option[Charset] = IOUtils.detectCharset(new ByteArrayInputStream(bytes), false)
      
      // The default charset for text should be Latin-1 according to http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7.1
      new String(bytes, charset.getOrElse(defaultEncoding))
    }
  }

  def readToStringWithCharset(encoding: Charset): Future[String] = readToStringWithCharset(encoding, Long.MaxValue)

  def readToStringWithCharset(encoding: Charset, maxLength: Long): Future[String] = {
    foldLeft(new StringBuilder){ (sb: StringBuilder, buf: ByteBuf) =>
      sb.append(buf.toString(encoding))
      require(sb.length <= maxLength, s"Body exceeds maxLength.  Body Length (so far): ${sb.length}  Specified Max Length: $maxLength")
      sb
    }.map{ _.toString }
  }
  
  /**
   * Write this body to a file
   */
  def writeToFile(file: File): Future[Unit] = writeToFile(file, Long.MaxValue)

  def writeToFile(file: File, maxLength: Long): Future[Unit] = {
    val os: FileOutputStream = new FileOutputStream(file)
    var bytesWritten: Long = 0
    val f: Future[Unit] = foldLeft(os){ (os, buf) =>
      val readablBytes: Int = buf.readableBytes()
      bytesWritten += readablBytes
      buf.readBytes(os, readablBytes)
      require(bytesWritten <= maxLength, s"Body exceeds maxLength.  Body Length (so far): $bytesWritten  Specified Max Length: $maxLength")
      os
    }.map{ _ => Unit }
    
    f.onComplete{
      case Success(_) => os.close()
      case Failure(_) =>
        os.close()
        file.delete()
    }
    
    f
  }
  
  /**
   * This is an asynchronous foreach where 'f' is called for each ByteBuf as the data is available.
   * @return A future is returned that is completed when 'f' has been called for each chunk.
   */
  def foreach[U](f: ByteBuf => U): Future[Unit] = foldLeft[Unit](()){ (_, buf) => f(buf) }.map{ _ => Unit }
  
  /**
   * This is an asynchronous foldLeft where op is called only when the chunks are ready.
   * @return The Result is returned as a Future
   */
  def foldLeft[B](z: B)(op: (B, ByteBuf) => B): Future[B] = synchronized {
    if (logger.isTraceEnabled) logger.trace(s"foldLeft - foldLeftCalled: ${foldLeftCalled.get}, current: $current")
    
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
    if (logger.isTraceEnabled) logger.trace("foldLeft0")
    
    ctx.read()
    
    chunk.onComplete{ t: Try[Option[LinkedHttpContent]] =>
      t match {
        case Failure(ex) => p.failure(ex)
        case Success(opt) => opt match {
          case None => hasBeenFullyRead.set(true); p.success(z)
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

  def discardContent(): Future[Unit] = {
    foldLeft(()){ (_, buf) => Unit }
  }

  // TODO: Does this really need to return a Future[Unit]?
  def close(): Unit = try {
    // This can throw an exception if foldLeft is also called
    // in a concurrent thread at the same time but that's okay.
    // What's NOT okay is trying to set foldLeftCalled from here
    // since it's set in foldLeft()
    if (!foldLeftCalled.get) discardContent() // TODO: is this safe since the foldLeft in discardContent is asynchronous?
  } catch {
    case ex: Exception => // ok
  }
}