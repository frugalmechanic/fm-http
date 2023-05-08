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

import fm.common.{ClassUtil, ImmutableDate, Logging}
import fm.common.Implicits._
import fm.common.JavaConverters._
import fm.http.{MimeTypes, MutableHeaders, Status}
import java.io.File
import java.net.{URL, URLConnection}
import scala.concurrent.duration._

object StaticClasspathFileHandler {
  def apply(root: String): StaticClasspathFileHandler = apply(new File(root), false, defaultClassLoader)
  def apply(root: String, classLoader: ClassLoader): StaticClasspathFileHandler = apply(new File(root), false, classLoader)
  
  def apply(root: File): StaticClasspathFileHandler = apply(root, false, defaultClassLoader)
  
  def apply(root: String, devMode: Boolean): StaticClasspathFileHandler = apply(new File(root), devMode, defaultClassLoader)
  def apply(root: String, devMode: Boolean, classLoader: ClassLoader): StaticClasspathFileHandler = apply(new File(root), devMode, classLoader)
  def apply(root: File, devMode: Boolean): StaticClasspathFileHandler = apply(root, devMode, defaultClassLoader)
  
  def apply(root: File, devMode: Boolean, classLoader: ClassLoader): StaticClasspathFileHandler = apply(Seq(root), devMode, classLoader)
  
  private def defaultClassLoader: ClassLoader = {
    val cl: ClassLoader = Thread.currentThread().getContextClassLoader()
    if (null != cl) cl else getClass.getClassLoader()
  }
}

final case class StaticClasspathFileHandler(roots: Seq[File], devMode: Boolean, classLoader: ClassLoader) extends StaticFileHandlerBase with Logging {
  
  private def getClasspathResource(f: File): Option[URL] = {
    if (null == f) return None
    
    val path: String = f.toString().stripLeading("/")
    val urls: Vector[URL] = classLoader.getResources(path).asScala.toVector
    
    if (urls.size > 1) {
      logger.warn(s"Found multiple resources that match '$path' : $urls")
      return None
    }
    
    urls.headOption
  }
  
  protected def isValidFile(f: File): Boolean = ClassUtil.classpathFileExists(f)
  
  protected def isValidDir(f: File): Boolean = ClassUtil.classpathDirExists(f)
  
  protected def lastModified(f: File): Long = ClassUtil.classpathLastModified(f)
  
  protected def handleNormal(request: Request, f: File, expirationSeconds: Int): Option[RequestHandler] = {
    val url: URL = getClasspathResource(f) match {
      case Some(u) => u
      case None => return None
    }
    
    // Optimization - Use the normal file system handling if the URL is a file 
    if (url.isFile) return handleFileSystemFile(request, url.toFile, expirationSeconds)
    
    val conn: URLConnection = url.openConnection()
    
    val headers: MutableHeaders = MutableHeaders()
    headers.date = ImmutableDate.now()
    
    val ifModifiedSince: Option[ImmutableDate] = request.headers.ifModifiedSince
    
    if (ifModifiedSince.exists{ _.millis === conn.getLastModified() }) {
      return Some(RequestHandler.constant(Response.NotModified(headers)))
    }
    
    // We don't want these headers returned with a 304 Not Modified response:
    // http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.5
    headers.contentType = MimeTypes.forFile(f).getOrElse(MimeTypes.BINARY)
    headers.lastModified = ImmutableDate(conn.getLastModified)
    headers.cacheControl = "public, max-age="+expirationSeconds
    headers.expires = ImmutableDate.now() + expirationSeconds.seconds
    
    val contentLength: Option[Long] = Some(conn.getContentLengthLong()).filter{ _ >= 0 }

    Some(RequestHandler.constant(InputStreamResponse(Status.OK, headers, conn.getInputStream(), contentLength)))
  }
}