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
import java.io.File
import java.nio.charset.{Charset, StandardCharsets}

object MimeTypes {
  // Images
  val BMP         = "image/bmp"
  val GIF         = "image/gif"
  val ICO         = "image/vnd.microsoft.icon"
  val JPEG        = "image/jpeg"
  val PNG         = "image/png"
  val SVG         = "image/svg+xml"
  val TIFF        = "image/tiff"
  val WEBP        = "image/webp"

  // Text
  val JAVASCRIPT  = "application/javascript"
  val JSON        = "application/json"
  val JSON_LD     = "application/ld+json"
  val CAL         = "text/calendar"
  val CSS         = "text/css"
  val CSV         = "text/csv"
  val HTML        = "text/html"
  val XHTML       = "application/xhtml+xml"
  val PLAIN       = "text/plain"
  val RTF         = "application/rtf"
  val XML         = "application/xml"

  // Other
  val `7ZIP`      = "application/x-7z-compressed"
  val BZIP        = "application/x-bzip"
  val BZIP2       = "application/x-bzip2"
  val GZIP        = "application/x-gzip"
  val RAR         = "application/x-rar-compressed"
  val ZIP         = "application/zip"
  val X_COMPONENT = "text/x-component"
  val RSS         = "application/rss+xml"
  val PDF         = "application/pdf"
  val BINARY      = "application/octet-stream"
  val SWF         = "application/x-shockwave-flash"
  val WOFF        = "application/font-woff"

  // Audio
  val AIFF        = "audio/x-aiff"
  val AU          = "audio/basic"
  val M3U         = "audio/x-mpegurl"
  val MIDI        = "audio/x-midi"
  val MP3         = "audio/mpeg"
  val OGG         = "audio/ogg"
  val WAV         = "audio/x-wav"
  val WEBA        = "audio/webm"

  // Video
  val `3GP`       = "video/3gpp"
  val `3G2`       = "video/3gpp2"
  val AVI         = "video/x-msvideo"
  val FLV         = "video/x-flv"
  val MOV         = "video/quicktime"
  val MP4         = "video/mp4"
  val M3U8        = "application/x-mpegURL"
  val OGV         = "video/ogg"
  val TS          = "video/MP2T"
  val WMV         = "video/x-ms-wmv"
  val WEBV        = "video/webm"

  // Microsoft Office Types
  val XLS         = "application/vnd.ms-excel"
  val XLSX        = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  val XLSM        = "application/vnd.ms-excel.sheet.macroEnabled.12"
  val DOC         = "application/msword"
  val DOCX        = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  val PPT         = "application/vnd.ms-powerpoint"
  val PPTX        = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  val VSD         = "application/vnd.visio"


  // type -> extensions
  val mimeTypeToExtension = Vector[(String,Seq[String])](
    // Images
    BMP         -> "bmp",
    GIF         -> "gif",
    ICO         -> "ico",
    JPEG        -> Seq("jpg","jpeg"),
    PNG         -> "png",
    SVG         -> "svg",
    TIFF        -> Seq("tif", "tiff"),
    WEBP        -> "webp",
    // Text
    JAVASCRIPT  -> "js",
    JSON        -> "json",
    JSON_LD     -> "jsonld",
    CAL         -> "ics",
    CSS         -> "css",
    CSV         -> "csv",
    HTML        -> Seq("html","htm"),
    XHTML       -> Seq("xhtml"),
    PLAIN       -> Seq("txt","tsv"),
    RTF         -> "rtf",
    XML         -> "xml",
    // Audio
    AU          -> Seq("au", "snd"),
    AIFF        -> Seq("aif", "aifc", "aiff"),
    M3U         -> "m3u",
    MIDI        -> Seq("mid", "rmi"),
    MP3         -> "mp3",
    OGG         -> Seq("ogg", "oga", "opus", "spx"),
    WAV         -> "wav",
    WEBA        -> "weba",
    // Video
    `3GP`       -> "3gp",
    `3G2`       -> "3g2",
    AVI         -> "avi",
    FLV         -> "flv",
    M3U8        -> "m3u8",
    MOV         -> "mov",
    MP4         -> "mp4",
    OGV         -> "ogv",
    TS          -> "ts",
    WMV         -> "wmv",
    WEBV        -> "webv",
    // Other
    `7ZIP`      -> "7z",
    BZIP        -> "bz",
    BZIP2       -> "bz2",
    GZIP        -> "gz",
    RAR         -> "rar",
    ZIP         -> "zip",
    X_COMPONENT -> "htc",
    RSS         -> "rss",
    PDF         -> "pdf",
    SWF         -> "swf",
    WOFF        -> "woff",
    BINARY      -> "ttf",
    // MS Office
    XLS         -> "xls",
    XLSX        -> "xlsx",
    XLSM        -> "xlsm",
    DOC         -> "doc",
    DOCX        -> "docx",
    PPT         -> "ppt",
    PPTX        -> "pptx",
    VSD         -> "vsd",
  ).toUniqueHashMap

  // type -> charset
  val mimeTypeToCharset = Vector[(String, Charset)](
    JSON -> StandardCharsets.UTF_8 // http://www.ietf.org/rfc/rfc4627.txt "JSON text SHALL be encoded in Unicode.  The default encoding is UTF-8."
  ).toUniqueHashMap

  val compressable: Vector[String] = Vector(HTML,JAVASCRIPT,JSON,CSS,CSV,PLAIN,XML,X_COMPONENT,RSS,SVG,XLS,DOC)

  // This is for the mimeTypeToExtension map
  private implicit def toSeq(s: String): Seq[String] = Seq(s)

  private val extensionToMimeType: Map[String, String] = {
    val builder = Vector.newBuilder[(String,String)]

    mimeTypeToExtension.foreach{ case (key,values) =>
      values.foreach{v => builder += v -> key }
    }

    builder.result.toUniqueHashMap
  }

  def forFile(f: File): Option[String] = getExtension(f.getName).flatMap{ forExtension }
  def forPath(path: String): Option[String] = getExtension(path).flatMap{ forExtension }
  def forExtension(extension: String): Option[String] = extensionToMimeType.get(extension.toLowerCase)

  def getExtension(name: String): Option[String] = {
    val idx: Int = name.lastIndexOf('.')
    if(-1 === idx) None else Some(name.substring(idx+1, name.length))
  }

  def isCompressable(contentType: String): Boolean = {
    if(null == contentType) return false
    compressable.exists{ contentType.startsWith }
  }
}
