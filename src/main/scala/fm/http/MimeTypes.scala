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

import java.io.File

object MimeTypes {
  val GIF = "image/gif"
  val JPEG = "image/jpeg"
  val PNG = "image/png"
  val JAVASCRIPT = "application/javascript"
  val JSON = "application/json"
  val CSS = "text/css"
  val CSV = "text/csv"
  val HTML = "text/html"
  val PLAIN = "text/plain"
  val XML = "application/xml"
  val GZIP = "application/x-gzip"
  val ZIP = "application/zip"
  val X_COMPONENT = "text/x-component"
  val RSS = "application/rss+xml"
  val SVG = "image/svg+xml"
  val BINARY = "application/octet-stream"

  // type -> extensions
  val mimeTypeToExtension = Map[String,Seq[String]](
    GIF         -> "gif",
    JPEG       -> Seq("jpg","jpeg"),
    PNG         -> "png",
    JAVASCRIPT  -> "js",
    CSS         -> "css",
    CSV         -> "csv",
    HTML        -> Seq("html","htm"),
    PLAIN       -> Seq("txt","tsv"),
    XML         -> "xml",
    GZIP        -> "gz",
    ZIP         -> "zip",
    X_COMPONENT -> "htc",
    SVG         -> "svg"
  )

  val compressable = Vector(HTML,JAVASCRIPT,JSON,CSS,CSV,PLAIN,XML,X_COMPONENT,RSS,SVG)

  private implicit def toSeq(s: String): Seq[String] = Seq(s)

  private val extensionToMimeType: Map[String, String] = {
    val builder = Map.newBuilder[String,String]

    mimeTypeToExtension.foreach{ case (key,values) =>
      values.foreach{v => builder += v -> key }
    }

    builder.result
  }

  def forFile(f: File): Option[String] = getExtension(f.getName).flatMap{forExtension}
  def forPath(path: String): Option[String] = getExtension(path).flatMap{forExtension}
  def forExtension(extension: String): Option[String] = extensionToMimeType.get(extension)

  def getExtension(name: String): Option[String] = {
    val idx = name.lastIndexOf('.')
    if(-1 == idx) None else Some(name.substring(idx+1, name.length))
  }

  def isCompressable(contentType: String): Boolean = {
    if(null == contentType) return false
    compressable.exists{contentType.startsWith}
  }
}
