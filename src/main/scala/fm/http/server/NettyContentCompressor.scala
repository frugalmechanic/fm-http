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

import io.netty.handler.codec.http.{HttpContentCompressor, HttpHeaders, HttpResponse}
import io.netty.handler.codec.http.HttpContentEncoder.Result

object NettyContentCompressor {
  /**
   * Do NOT compress these types
   * 
   * Note: This only applies if there are "text/" types that we do not want to compress
   * Note: Needs to be lower case
   */
  private val blacklist: Set[String] = Set()
  
  /**
   * It's okay to compress these types
   * 
   * Note: Needs to be lower case
   */
  private val whitelist: Set[String] = Set(
    "application/javascript",
    "application/json",
    "application/xml"
  )
  
  def isCompressable(response: HttpResponse): Boolean = {
    val contentType: String = response.headers().get(HttpHeaders.Names.CONTENT_TYPE)
    isCompressable(contentType)
  }
  
  /**
   * We assume anything that starts with "text/" can probably be compressed
   */
  private def isCompressable(contentType: String): Boolean = {
    if (null == contentType) return false
    
    val cleaned: String = cleanContentType(contentType)
    
    if (blacklist.contains(cleaned)) false          // Don't allow anything on the blacklist
    else if (contentType.startsWith("text/")) true  // Anything that starts with "text/" is probably okay
    else whitelist.contains(cleaned)                // Anything on the whitelist is also okay
  }
  
  private def cleanContentType(tpe: String): String = {
    // Examples:
    // 
    // Content-Type: text/html
    // Content-Type: text/html;charset=utf8
    // Content-Type: TEXT/HTML;charset=utf8
    //
    val idx: Int = tpe.indexOf(';')
    val cleaned: String = if (idx != -1) tpe.substring(0, idx) else tpe
    cleaned.trim().toLowerCase()
  }
}

/**
 * A Mime-Type aware HttpContentCompressor that won't compress content that shouldn't be compressed
 */
final class NettyContentCompressor extends HttpContentCompressor {
  import NettyContentCompressor.isCompressable
  
  override protected def beginEncode(response: HttpResponse, acceptEncoding: String): Result = {
    // If this is not compressable content then we just return null for no encoding
    if (isCompressable(response)) super.beginEncode(response, acceptEncoding) else null
  }
  
}