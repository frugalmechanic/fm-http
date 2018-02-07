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

import io.netty.buffer.{ByteBuf, ByteBufAllocator}
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{DefaultHttpContent, HttpContent, LastHttpContent}
import io.netty.handler.stream.ChunkedInput

/**
 * Takes a ChunkedInput[ByteBuf] and turns it into a ChunkedInput[HttpContent] so the NettyContentCompressor works
 */
final case class HttpContentChunkedInput(input: ChunkedInput[ByteBuf]) extends ChunkedInput[HttpContent] {
  def close(): Unit = input.close()
  
  def isEndOfInput(): Boolean = input.isEndOfInput()

  def length(): Long = input.length()

  def progress(): Long = input.progress()

  /**
   * Fetches a chunked data from the stream. Once this method returns the last chunk and
   * thus the stream has reached at its end, any subsequent isEndOfInput() call must
   * return false.
   *
   * @returns the fetched chunk. null if there is no data left in the stream. Please
   *          note that null does not necessarily mean that the stream has reached at
   *          its end. In a slow stream, the next chunk might be unavailable just momentarily.
   */
  def readChunk(allocator: ByteBufAllocator): HttpContent = {
    if (isEndOfInput) return LastHttpContent.EMPTY_LAST_CONTENT
    val buf: ByteBuf = input.readChunk(allocator)
    if (null == buf) null else new DefaultHttpContent(buf)
  }

  /**
   * Fetches a chunked data from the stream. Once this method returns the last chunk and 
   * thus the stream has reached at its end, any subsequent isEndOfInput() call must 
   * return false.
   * 
   * @returns the fetched chunk. null if there is no data left in the stream. Please 
   *          note that null does not necessarily mean that the stream has reached at 
   *          its end. In a slow stream, the next chunk might be unavailable just momentarily.
   */
  def readChunk(ctx: ChannelHandlerContext): HttpContent = readChunk(ctx.alloc())
}