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

import fm.common.Implicits._
import fm.common.ScheduledTaskRunner
import fm.http._
import java.io.Closeable
import java.nio.charset.{Charset, StandardCharsets}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

/**
 * This holds a single copy of the NioEventLoopGroup / NettyExecutionContext 
 */
object HttpClient {

  /**
   * This can be imported if you know what you are doing
   */
  implicit val executionContext: ExecutionContext = HttpExecutionContext.global
  
  /**
   * This can be used to schedule tasks
   */
  implicit val timer: ScheduledTaskRunner = HttpExecutionContext.timer
  
  def apply(
    socksProxy: Option[(String, Int)] = None,
    defaultMaxLength: Long = 10485760, /* 10MB (which may or may not be 10MB worth of Chars) */
    defaultHeaders: Headers = DefaultHeaders,
    useConnectionPool: Boolean = true,   // Should we re-use connections? (Use HTTP Keep Alive?)
    maxConnectionsPerHost: Int = 8,      // Only applies if useConnectionPool is true
    maxRequestQueuePerHost: Int = 1024,  // Only applies if useConnectionPool is true
    maxConnectionIdleDuration: FiniteDuration = 30.seconds,
    defaultResponseTimeout: Duration = 5.minutes, // The maximum time to wait for a Response
    defaultConnectTimeout: Duration = 30.seconds, // The maximum time to wait to connect to a server
    defaultCharset: Charset = StandardCharsets.ISO_8859_1, // The default charset to use (if none is specified in the response) when converting responses to strings
    maxRedirectCount: Int = 5, // The maximum number of 301/302 redirects to follow for a GET or HEAD request
    followRedirects: Boolean = true
  ): DefaultHttpClient = DefaultHttpClient(
    socksProxy = socksProxy,
    defaultMaxLength = defaultMaxLength,
    defaultHeaders = defaultHeaders,
    useConnectionPool = useConnectionPool,
    maxConnectionsPerHost = maxConnectionsPerHost,
    maxRequestQueuePerHost = maxRequestQueuePerHost,
    maxConnectionIdleDuration = maxConnectionIdleDuration,
    defaultResponseTimeout = defaultResponseTimeout,
    defaultConnectTimeout = defaultConnectTimeout,
    defaultCharset = defaultCharset,
    maxRedirectCount = maxRedirectCount,
    followRedirects = followRedirects
  )
  
  private val DefaultHeaders: ImmutableHeaders = {
    val h: MutableHeaders = MutableHeaders()
    h.accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
    h.acceptEncoding = "gzip,deflate"
    h.acceptLanguage = "en-US,en;q=0.8"
    h.userAgent = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)"
    h.toImmutableHeaders
  }
}

abstract class HttpClient extends Closeable { 
  
  final implicit def executionContext: ExecutionContext = HttpClient.executionContext
  final implicit def timer: ScheduledTaskRunner = HttpClient.timer
  
  def defaultMaxLength: Long
  def defaultHeaders: Headers
  def defaultResponseTimeout: Duration
  def defaultCharset: Charset
  
  /**
   * Execute a Request returning the AsyncResponse
   */
  def execute(r: Request, timeout: Duration): Future[AsyncResponse]
  
  def close(): Unit
  
  /**
   * Perform a HEAD request.  Always returns a FullResponse because the body will be empty
   */
  final def head(url: String, headers: Headers = defaultHeaders, timeout: Duration = defaultResponseTimeout): Future[FullResponse] = execute(Request.Head(url, headers), timeout).flatMap{ _.toFullResponse(0) }
  
  /**
   * Perform a GET request returning the FullResponse with a max length for the body
   */
  final def getFull(url: String, headers: Headers = defaultHeaders, maxLength: Long = defaultMaxLength, timeout: Duration = defaultResponseTimeout, defaultCharset: Charset = defaultCharset): Future[FullResponse] = getAsync(url, headers, timeout).flatMap{ _.toFullResponse(maxLength, defaultCharset) }

  /**
   * Perform a GET request returning the FullBytesResponse with a max length for the bytes
   */
  final def getFullBytes(url: String, headers: Headers = defaultHeaders, maxLength: Long = defaultMaxLength, timeout: Duration = defaultResponseTimeout): Future[FullBytesResponse] = getAsync(url, headers, timeout).flatMap{ _.toFullBytesResponse(maxLength) }

  /**
   * Perform a POST request returning the FullResponse with a max length for the body
   */
  final def postFull(url: String, body: String, headers: Headers = defaultHeaders, maxLength: Long = defaultMaxLength, timeout: Duration = defaultResponseTimeout, defaultCharset: Charset = defaultCharset): Future[FullResponse] = postAsync(url, body, headers, timeout).flatMap{ _.toFullResponse(maxLength, defaultCharset) }
  
  /**
   * Perform a GET request returning an AsyncResponse for reading arbitrarily long response bodies
   */
  final def getAsync(url: String, headers: Headers = defaultHeaders, timeout: Duration = defaultResponseTimeout): Future[AsyncResponse] = execute(Request.Get(url, headers), timeout)
  
  /**
   * Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies
   */
  final def postAsync(url: String, body: String, headers: Headers = defaultHeaders, timeout: Duration = defaultResponseTimeout): Future[AsyncResponse] = execute(Request.Post(url, headers, body), timeout)

  /**
   * Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies, using a Byte array as the post body
   */
  final def postBytesAsync(url: String, array: Array[Byte], headers: Headers = defaultHeaders, timeout: Duration = defaultResponseTimeout): Future[AsyncResponse] = execute(Request.Post(url, headers, array), timeout)

  /**
   * Return an HttpClient that will use Basic auth for any calls made by it
   */
  final def withBasicAuth(user: String, pass: String): HttpClient = new BasicAuthHttpClient(user, pass, this)
  
  /**
   * Return an HttpClient that will use Digest auth for any calls made by it
   */
  final def withDigestAuth(user: String, pass: String): HttpClient = new DigestAuthHttpClient(user, pass, this)
}
