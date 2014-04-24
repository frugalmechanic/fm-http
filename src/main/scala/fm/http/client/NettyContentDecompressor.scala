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

import io.netty.channel.embedded.EmbeddedChannel
import io.netty.handler.codec.compression.{JdkZlibDecoder, ZlibWrapper}
import io.netty.handler.codec.http.HttpContentDecoder

/**
 * Clone of the HttpContentDecompressor without the dependency on http://www.jcraft.com/jzlib/
 */
final class NettyContentDecompressor() extends HttpContentDecoder {
  // Must be true for the JdkZlibDecoder implementation
  private val strict: Boolean = true
  
  override protected def newContentDecoder(contentEncoding: String): EmbeddedChannel = {
    if ("gzip".equalsIgnoreCase(contentEncoding) || "x-gzip".equalsIgnoreCase(contentEncoding)) {
      new EmbeddedChannel(new JdkZlibDecoder(ZlibWrapper.GZIP))
    } else if ("deflate".equalsIgnoreCase(contentEncoding) || "x-deflate".equalsIgnoreCase(contentEncoding)) {
      val wrapper: ZlibWrapper = if (strict) ZlibWrapper.ZLIB else ZlibWrapper.ZLIB_OR_NONE
      // To be strict, 'deflate' means ZLIB, but some servers were not implemented correctly.
      new EmbeddedChannel(new JdkZlibDecoder(wrapper))
    } else {
      // 'identity' or unsupported
      null
    }
  }
}