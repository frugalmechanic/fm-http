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

import fm.common.Implicits._
import fm.http.{MimeTypes, MutableHeaders, Status}
import java.io.File
import java.util.regex.{Pattern, Matcher}
import io.netty.handler.codec.http.HttpMethod
import org.joda.time.DateTime

trait StaticFileHandlerBase extends RequestRouter {
  import StaticFileHandler.{expirationInSeconds, formattedTimestamp, indexFiles, versionedExprationInSeconds}
  
  private val HasTimestampPattern: Pattern = Pattern.compile(""".+\.[0-9]+\.\w+$""")
  private val HasTimestampOrHashPattern: Pattern = Pattern.compile(""".+\.[0-9a-zA-Z]{8,}+\.\w+$""")
  
  protected sealed trait ResolvedFile { def file: File }
  protected case class NormalResolvedFile(file: File, expirationSeconds: Int) extends ResolvedFile
  protected case class RedirectResolvedFile(file: File, location: String) extends ResolvedFile
  
  /** Root paths to search (in order) to find a matching file */
  protected def roots: Seq[File]
  
  /** Is this a valid file we can serve? */
  protected def isValidFile(f: File): Boolean

  /**
   * Is this a valid directory that we should check for an index file?
   * 
   * Note: If you don't want to use index files then this can just return false for everything.
   */
  protected def isValidDir(f: File): Boolean
  
  /**
   * Last modified timestamp for a file in milliseconds
   */
  protected def lastModified(f: File): Long
  
  protected def handleNormal(request: Request, f: File, expirationSeconds: Int): Option[RequestHandler]
  
  /**
   * Given a file path return the timestamped version of the path.
   * 
   * Note: If the file doesn't exist then the path is returned unmodified
   * 
   * e.g. Given "/js/foo.js" return something like "/js/foo.1470690999454.js" (assuming the file exists)
   */
  final def timestampedPath(path: String): String = {
    // If there is already a timestamp then don't modify it
    if (path.matches(HasTimestampPattern) || !path.contains('.')) return path
    
    // Otherwise we attempt to resolve the file and add the timestamp
    val pathWithTimestamp: Option[String] = for {
      resolved: ResolvedFile <- findResolvedFile(path)
      file: File = resolved.file
      timestamp: Long = lastModified(file)
      if timestamp > 0
      formattedTimestamp: String = StaticFileHandler.formattedTimestamp(timestamp)
    } yield {
      val dotIdx: Int = path.lastIndexOf('.')
      val prefix: String = path.substring(0, dotIdx)
      val suffix: String = path.substring(dotIdx+1)
      s"$prefix.$formattedTimestamp.$suffix"
    }
    
    pathWithTimestamp.getOrElse{ path }
  }
  
  final protected def isFileSystemFile(f: File): Boolean = null != f && f.isFile && f.canRead && !f.isHidden
  
  final protected def isFileSystemDir(f: File): Boolean = null != f && f.isDirectory && f.canRead && !f.isHidden
  
  /**
   * This can be called by handleNormal for when the File refers to an actual file on the file system
   */
  protected def handleFileSystemFile(request: Request, f: File, expirationSeconds: Int): Option[RequestHandler] = {
    if (!isFileSystemFile(f)) return None
    
    val headers: MutableHeaders = MutableHeaders()
    headers.date = DateTime.now
    
    val ifModifiedSince: Option[DateTime] = request.headers.ifModifiedSince
    
    if (ifModifiedSince.exists{ _.getMillis() === f.lastModified() }) {
      return Some(RequestHandler.constant(Response.NotModified(headers)))
    }
    
    // We don't want these headers returned with a 304 Not Modified response:
    // http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.5
    headers.contentType = MimeTypes.forFile(f).getOrElse(MimeTypes.BINARY)
    headers.lastModified = new DateTime(f.lastModified())
    headers.cacheControl = "public, max-age="+expirationSeconds
    headers.expires = DateTime.now().plusSeconds(expirationSeconds)

    Some(RequestHandler.constant(FileResponse(Status.OK, headers, f)))
  }
  
  final def lookup(request: Request): Option[RequestHandler] = {
    // We only handle GET and HEAD requests
    if (request.method != HttpMethod.GET && request.method != HttpMethod.HEAD) return None
    
    val resolved: Option[ResolvedFile] = findResolvedFile(request)
    
    if (resolved.isEmpty) None
    else resolved.get match {
      case NormalResolvedFile(file, expirationSeconds) => handleNormal(request, file, expirationSeconds)
      case RedirectResolvedFile(file, location)        => Some(RequestHandler.constant(Response.Found(location)))
    }
  }
  
  private def findResolvedFile(request: Request): Option[ResolvedFile] = findResolvedFile(request.path, request.uri)
  private def findResolvedFile(path: String): Option[ResolvedFile] = findResolvedFile(path, path)
  
  /**
   * Checks each root (in order) attempting to resolve the path to a ResolvedFile
   */
  private def findResolvedFile(path: String, uri: String): Option[ResolvedFile] = {
    if (path.isBlank) return None
    
    val parts: Array[String] = path.split('/')
    
    // We don't allow dotfiles (e.g. ".foo") or stuff like "." or ".."
    if (parts.exists{ p: String => p.startsWith(".") }) return None

    roots.findMapped{ root: File =>
      val f: File = new File(root, parts.mkString(File.separator))
      tryResolveFile(path, uri, f)
    }
  }

  private def tryResolveFile(request: Request, f: File): Option[ResolvedFile] = tryResolveFile(request.path, request.uri, f)
  private def tryResolveFile(path: String, f: File): Option[ResolvedFile] = tryResolveFile(path, path, f)
  
  /**
   * Given a file check for possible alternates if it's not a valid file:
   *  - Index File if it's a directory
   *  - Timestamped suffix:  /javascripts/cached/all.20110614092900.js
   */
  private def tryResolveFile(path: String, uri: String, f: File): Option[ResolvedFile] = {
    // The file is already valid
    if (isValidFile(f)) {
      // Does it look like a versioned file?  name.{timestamp|md5|sha1}.ext
      // This will get triggered for the SmartSprite images that look like: /images/sprites/common.43babe75d882962f82f24ed81ec179cc.png
      val isVersioned: Boolean = f.getName.matches(HasTimestampOrHashPattern)
      val expiration: Int = if (isVersioned) versionedExprationInSeconds else expirationInSeconds
      return Some(NormalResolvedFile(f, expiration))
    }

    // Directory -- Check for index files
    if (isValidDir(f)) {
      // Check for index files
      val idxFile: File = indexFiles.map{ idxPath: String => new File(f, idxPath)}.find{ isValidFile }.orNull

      if (isValidFile(idxFile)) {
        // If we are serving up an index file make sure we have a trailing slash on the request
        if (!path.endsWith("/")) {
          val location: String = uri.replaceFirst(Pattern.quote(path), Matcher.quoteReplacement(path+"/"))
          return Some(RedirectResolvedFile(idxFile, location))
        }

        return Some(NormalResolvedFile(idxFile, expirationInSeconds))
      }
    }

    // Timestamped/MD5'd resource file:  e.g. /javascripts/cached/all.20110614092900.js
    // TODO: make this work with MD5/SHA1
    if (null != f && f.getName.matches(HasTimestampPattern)) {
      val splitParts: Array[String] = f.getAbsolutePath.split('.')
      val timestampIdx: Int = splitParts.length-2

      // Grab the timestamp
      val timestamp: String = splitParts(timestampIdx)

      // null out the timestamp
      splitParts(timestampIdx) = null

      // rejoin without the timestamp
      val newPath: String = splitParts.filter{ _ != null }.mkString(".")
      val newFile: File = new File(newPath)
      // only use this file if it exists
      if (isValidFile(newFile)) {
       
        // If the timestamps don't match then 302 to whatever timestamp the real file has.  We don't
        // want to serve up the wrong file for whatever the requested timestamp is.
        val expectedTimestamp: String = formattedTimestamp(newFile)
        if (timestamp != expectedTimestamp) {

          // TODO: fix this up since it isn't very fullproof replacement...
          val location: String = uri.replaceFirst(Pattern.quote("."+timestamp+"."), Matcher.quoteReplacement("."+expectedTimestamp+"."))

          return Some(RedirectResolvedFile(newFile, location))
        }
        
        return Some(NormalResolvedFile(newFile, versionedExprationInSeconds))
      }
    }

    None
  }
}