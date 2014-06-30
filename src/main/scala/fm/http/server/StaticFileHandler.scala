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

import fm.http.{MimeTypes, MutableHeaders, Status}
import java.io.File
import org.joda.time.{DateTime, LocalDateTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

object StaticFileHandler {
  /** Normal expiration for static assets */
  val expirationInSeconds: Int = 86400 // 1 day
  
  /** If an asset is versioned (timestamp/md5/sha1) then it's safe to use a longer expiration */
  val versionedExprationInSeconds: Int = 31536000 // 365 days

  /** Default indexFiles to use */
  val indexFiles: Seq[String] = Seq("index.html")

  private[this] val timestampFormat: DateTimeFormatter = DateTimeFormat.forPattern("yyyyMMddHHmmss")
  
  def formattedTimestamp(f: File): String = new LocalDateTime(f.lastModified()).toString(timestampFormat)

  def apply(root: String): StaticFileHandler = StaticFileHandler(new File(root))
}

final case class StaticFileHandler(root: File) extends StaticFileHandlerBase {
  protected def isValidFile(f: File): Boolean = isFileSystemFile(f)
  
  protected def isValidDir(f: File): Boolean = isFileSystemDir(f)
  
  protected def handleNormal(request: Request, f: File, expirationSeconds: Int): Option[RequestHandler] = handleFileSystemFile(request, f, expirationSeconds)
}