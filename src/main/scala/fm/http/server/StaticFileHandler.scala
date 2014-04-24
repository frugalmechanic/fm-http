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

import java.io.File
import java.util.Date
import java.util.regex.{Pattern, Matcher}
import org.joda.time.{DateTime, LocalDateTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import io.netty.handler.codec.http.{DefaultHttpHeaders, HttpHeaders, HttpMethod}
import scala.concurrent.Future
import fm.common.Implicits._
import fm.http.{MimeTypes, MutableHeaders, Status}

object StaticFileHandler {
  /** Normal expiration for static assets */
  val expirationInSeconds: Int = 86400 // 1 day
  
  /** If an asset is versioned (timestamp/md5/sha1) then it's safe to use a longer expiration */
  val versionedExprationInSeconds: Int = 31536000 // 365 days

  /** Default indexFiles to use */
  val indexFiles: Seq[String] = Seq("index.html")

  private[this] val timestampFormat: DateTimeFormatter = DateTimeFormat.forPattern("yyyyMMddHHmmss")
  
  def formattedTimestamp(f: File): String = new LocalDateTime(f.lastModified()).toString(timestampFormat)

  private sealed trait ResolvedFile
  private case class NormalResolvedFile(file: File, expirationSeconds: Int) extends ResolvedFile
  private case class RedirectResolvedFile(location: String) extends ResolvedFile
  
}

final case class StaticFileHandler(root: File) extends RequestRouter {
  import StaticFileHandler._
  
  
  private def isValidFile(f: File): Boolean = null != f && f.isFile && f.canRead && !f.isHidden
  
  private def isValidDir(f: File): Boolean = null != f && f.isDirectory && f.canRead && !f.isHidden
  
  def lookup(request: Request): Option[RequestHandler] = {
    // We only handle GET and HEAD requests
    if (request.method != HttpMethod.GET && request.method != HttpMethod.HEAD) return None
    
    toFile(request.path).map{ resolveFile(request, _) }.flatMap { _ match {
      case NormalResolvedFile(file, expirationSeconds) => handleNormal(request, file, expirationSeconds)
      case RedirectResolvedFile(location)              => Some(RequestHandler.constant(Response.Found(location)))
    }}
  }
  
  private def handleNormal(request: Request, f: File, expirationSeconds: Int): Option[RequestHandler] = {
    if(!isValidFile(f)) return None
    
    val headers: MutableHeaders = MutableHeaders()
    headers.date = DateTime.now
    headers.contentType = MimeTypes.forFile(f).getOrElse(MimeTypes.BINARY)
    headers.lastModified = new DateTime(f.lastModified())
    headers.cacheControl = "public, max-age="+expirationSeconds
    headers.expires = DateTime.now().plusSeconds(expirationSeconds)
    
    val ifModifiedSince: Option[DateTime] = request.headers.ifModifiedSince
    
    if (ifModifiedSince.exists{ _.getMillis() == f.lastModified() }) {
      return Some(RequestHandler.constant(Response.NotModified(headers)))
    }

    Some(RequestHandler.constant(FileResponse(Status.OK, headers, f)))
  }
  
  /**
   * Given a file check for possible alternates if it's not a valid file:
   *  - Index File if it's a directory
   *  - Timestamped suffix:  /javascripts/cached/all.20110614092900.js
   */
  private def resolveFile(request: Request, f: File): ResolvedFile = {
    // The file is already valid
    if(isValidFile(f)) {
      // Does it look like a versioned file?  name.{timestamp|md5|sha1}.ext
      // This will get triggered for the SmartSprite images that look like: /images/sprites/common.43babe75d882962f82f24ed81ec179cc.png
      val isVersioned: Boolean = f.getName.matches(""".+\.[0-9a-zA-Z]{8,}+\.\w+$""")
      val expiration: Int = if (isVersioned) versionedExprationInSeconds else expirationInSeconds
      return NormalResolvedFile(f, expiration)
    }

    // Directory -- Check for index files
    if(isValidDir(f)) {
      // Check for index files
      val idxFile: File = indexFiles.map{ idxPath: String => new File(f, idxPath)}.find{ isValidFile }.orNull

      if(isValidFile(idxFile)) {
        // If we are serving up an index file make sure we have a trailing slash on the request
        if(!request.path.endsWith("/")) {
          val location: String = request.uri.replaceFirst(Pattern.quote(request.path), Matcher.quoteReplacement(request.path+"/"))
          return RedirectResolvedFile(location)
        }

        return NormalResolvedFile(idxFile, expirationInSeconds)
      }
    }

    // Timestamped/MD5'd resource file:  e.g. /javascripts/cached/all.20110614092900.js
    // TODO: make this work with MD5/SHA1
    if(null != f && f.getName.matches(""".+\.[0-9]+\.\w+$""")) {
      val splitParts = f.getAbsolutePath.split('.')
      val timestampIdx = splitParts.length-2

      // Grab the timestamp
      val timestamp: String = splitParts(timestampIdx)

      // null out the timestamp
      splitParts(timestampIdx) = null

      // rejoin without the timestamp
      val newPath: String = splitParts.filter{_ != null}.mkString(".")
      val newFile: File = new File(newPath)
      // only use this file if it exists
      if(isValidFile(newFile)) {
       
        // If the timestamps don't match then 302 to whatever timestamp the real file has.  We don't
        // want to serve up the wrong file for whatever the requested timestamp is.
        val expectedTimestamp: String = formattedTimestamp(newFile)
        if(timestamp != expectedTimestamp) {

          // TODO: fix this up since it isn't very fullproof replacement...
          val location = request.uri.replaceFirst(Pattern.quote("."+timestamp+"."), Matcher.quoteReplacement("."+expectedTimestamp+"."))

          return RedirectResolvedFile(location)
        }
        
        return NormalResolvedFile(newFile, versionedExprationInSeconds)
      }
    }

    NormalResolvedFile(f, -1)
  }
  
  /**
   * Convert a request.path into a File
   */
  private def toFile(path: String): Option[File] = {
    if (path.isBlank) return None
    
    val parts: Array[String] = path.split('/')
    
    // We don't allow dotfiles (e.g. ".foo") or stuff like "." or ".."
    if (parts.exists{ p: String => p.startsWith(".") }) return None

    Some(new File(root, parts.mkString(File.separator)))
  }
}