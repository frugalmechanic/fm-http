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

import fm.common.{IOUtils, Logging, StacklessException}
import fm.common.Implicits._
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{DefaultFullHttpResponse, FullHttpResponse, HttpResponseStatus, HttpVersion}
import io.netty.util.CharsetUtil
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, Closeable, File, FileOutputStream}
import java.nio.charset.Charset
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object LinkedHttpContentReader extends Logging {
  def apply(need100Continue: Boolean, contentLength: Option[Long], head: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext, executor: ExecutionContext): LinkedHttpContentReader = new LinkedHttpContentReader(need100Continue, contentLength, head)

  def empty(implicit executor: ExecutionContext): LinkedHttpContentReader = {
    val ctx: ChannelHandlerContext = null
    LinkedHttpContentReader(false, None, Future.successful(None))(ctx, executor)
  }

  sealed trait MaxLengthStrategy {
    def checkMaxLengthAfterRead(currentLength: Long, maxLength: Long): Unit = {
      if ((this === MaxLengthStrategy.CloseConnection) && currentLength > maxLength) throw new MaxLengthException(s"Body exceeds maxLength.  Body Length (so far): $currentLength  Specified Max Length: $maxLength")
    }

    // This is just an alias of checkMaxLengthOnComplete, but helpful for reading the code.
    def checkMaxLengthFromContentLength(contentLength: Long, maxLength: Long): Unit = checkMaxLengthAfterAllContentRead(contentLength, maxLength)

    def checkMaxLengthAfterAllContentRead(bodyLength: Long, maxLength: Long): Unit = {
      if ((this === MaxLengthStrategy.DiscardAndThrowException) &&  bodyLength > maxLength) throw new MaxLengthException(s"Body exceeds maxLength. Body Length: $bodyLength  Specified Max Length: $maxLength")
    }
  }

  case class MaxLengthException(msg: String) extends StacklessException(msg)

  object MaxLengthStrategy {
    // Throws an exception immediately when limit reached
    case object CloseConnection          extends MaxLengthStrategy
    // No exception, but response is truncated to the limit
    case object Truncate                 extends MaxLengthStrategy
    // Reads the entire linked reader but starts discarding content after the max limit, and then throws exception after fully read.
    case object DiscardAndThrowException extends MaxLengthStrategy
  }

  private val CONTINUE: FullHttpResponse = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE, Unpooled.EMPTY_BUFFER)  
}

final class LinkedHttpContentReader(is100ContinueExpected: Boolean, contentLength: Option[Long], head: Future[Option[LinkedHttpContent]])(implicit ctx: ChannelHandlerContext, executor: ExecutionContext) extends Closeable with Logging {
  import LinkedHttpContentReader.CONTINUE
  import LinkedHttpContentReader.MaxLengthStrategy

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
  def readToByteArray(maxLength: Long): Future[Array[Byte]] = readToByteArray(maxLength, MaxLengthStrategy.CloseConnection)
  
  def readToByteArray(maxLength: Long, maxLengthStrategy: MaxLengthStrategy): Future[Array[Byte]] = {
    contentLength.foreach{ maxLengthStrategy.checkMaxLengthFromContentLength(_, maxLength) }

    val bytesRead: AtomicLong = new AtomicLong(0)

    foldLeft(new ByteArrayOutputStream){ (out: ByteArrayOutputStream, buf: ByteBuf) =>
      val readableBytes: Int = buf.readableBytes
      bytesRead.addAndGet(readableBytes)
      buf.readBytes(out, buf.readableBytes())
      maxLengthStrategy.checkMaxLengthAfterRead(out.size, maxLength)

      out
    }.filter { _ =>
      maxLengthStrategy.checkMaxLengthAfterAllContentRead(bytesRead.get, maxLength)

      true
    }.map{ _.toByteArray }
  }
  
  /**
   * Read the response body into a string
   */
  def readToString(maxLength: Long): Future[String] = readToString(maxLength, CharsetUtil.ISO_8859_1, MaxLengthStrategy.CloseConnection)
  def readToString(maxLength: Long, maxLengthStrategy: MaxLengthStrategy): Future[String] = readToString(maxLength, CharsetUtil.ISO_8859_1, maxLengthStrategy)
    
  def readToString(encoding: Charset): Future[String] = readToString(Long.MaxValue, encoding, MaxLengthStrategy.CloseConnection)

  def readToString(maxLength: Long, encoding: Charset): Future[String] = readToString(maxLength, encoding, MaxLengthStrategy.CloseConnection)

  def readToString(maxLength: Long, encoding: Charset, maxLengthStrategy: MaxLengthStrategy): Future[String] = {
    if (null == encoding) readToStringWithDetectedCharset(maxLength, maxLengthStrategy) else readToStringWithCharset(encoding, maxLength, maxLengthStrategy)
  }

  def readToStringWithDetectedCharset(): Future[String] = readToStringWithDetectedCharset(Long.MaxValue)
  def readToStringWithDetectedCharset(maxLength: Long): Future[String] = readToStringWithDetectedCharset(maxLength, CharsetUtil.ISO_8859_1)
  def readToStringWithDetectedCharset(maxLength: Long, maxLengthStrategy: MaxLengthStrategy): Future[String] = readToStringWithDetectedCharset(maxLength, CharsetUtil.ISO_8859_1, maxLengthStrategy)
  def readToStringWithDetectedCharset(maxLength: Long, defaultEncoding: Charset): Future[String] = readToStringWithDetectedCharset(maxLength, defaultEncoding, MaxLengthStrategy.CloseConnection)

  def readToStringWithDetectedCharset(maxLength: Long, defaultEncoding: Charset, maxLengthStrategy: MaxLengthStrategy): Future[String] = {
    readToByteArray(maxLength, maxLengthStrategy).map{ bytes: Array[Byte] =>
      val charset: Option[Charset] = IOUtils.detectCharset(new ByteArrayInputStream(bytes), false)
      
      // The default charset for text should be Latin-1 according to http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7.1
      new String(bytes, charset.getOrElse(defaultEncoding))
    }
  }

  def readToStringWithCharset(encoding: Charset): Future[String] = readToStringWithCharset(encoding, Long.MaxValue)
  def readToStringWithCharset(encoding: Charset, maxLength: Long): Future[String] = readToStringWithCharset(encoding, maxLength, MaxLengthStrategy.CloseConnection)

  def readToStringWithCharset(encoding: Charset, maxLength: Long, maxLengthStrategy: MaxLengthStrategy): Future[String] = {
    contentLength.foreach{ maxLengthStrategy.checkMaxLengthFromContentLength(_, maxLength) }

    foldLeft(new StringBuilder){ (sb: StringBuilder, buf: ByteBuf) =>
      if (sb.length <= maxLength) sb.append(buf.toString(encoding))
      else buf.discardReadBytes()


      maxLengthStrategy.checkMaxLengthAfterRead(sb.length, maxLength)

      sb
    }.filter{ sb: StringBuilder =>
      maxLengthStrategy.checkMaxLengthAfterAllContentRead(sb.length, maxLength)
      true
    }.map{ _.toString }
  }

  /**
   * Write this body to a file
   */
  def writeToFile(file: File): Future[Unit] = writeToFile(file, Long.MaxValue, MaxLengthStrategy.CloseConnection)

  def writeToFile(file: File, maxLength: Long, maxLengthStrategy: MaxLengthStrategy): Future[Unit] = {
    import java.util.concurrent.atomic.AtomicLong
    contentLength.foreach{ maxLengthStrategy.checkMaxLengthFromContentLength(_, maxLength) }

    val os: FileOutputStream = new FileOutputStream(file)
    val bytesRead: AtomicLong = new AtomicLong(0)

    val f: Future[Unit] = {
      foldLeft(os){ (os, buf) =>
        val readableBytes: Int = buf.readableBytes
        bytesRead.addAndGet(readableBytes)

        maxLengthStrategy.checkMaxLengthAfterRead(bytesRead.get, maxLength)
        if (bytesRead.get <= maxLength) buf.readBytes(os, readableBytes)
        else buf.discardReadBytes()

        os
      }.map{ _ => Unit }
    }
    
    f.onComplete{ case _ =>
      os.close()
    }

    f.filter{ _: Unit =>
      maxLengthStrategy.checkMaxLengthAfterAllContentRead(bytesRead.get, maxLength)

      true
    }
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