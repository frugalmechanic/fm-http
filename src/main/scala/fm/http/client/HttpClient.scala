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

import fm.common.ScheduledTaskRunner
import fm.http._
import java.io.{Closeable, File}
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
    followRedirects: Boolean = true, // Should 301/302 redirects be followed for GET or HEAD requests?
    maxRedirectCount: Int = 5  // The maximum number of 301/302 redirects to follow for a GET or HEAD request if followRedirects is true
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
    followRedirects = followRedirects,
    maxRedirectCount = maxRedirectCount
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
  
  /** Return an HttpClient that will use Basic auth for any calls made by it */
  final def withBasicAuth(user: String, pass: String): HttpClient = new BasicAuthHttpClient(user, pass, this)
  
  /** Return an HttpClient that will use Digest auth for any calls made by it */
  final def withDigestAuth(user: String, pass: String): HttpClient = new DigestAuthHttpClient(user, pass, this)
  
  //
  // Implementations used by the generated code below
  //
  private def headImpl(url: String, headers: Headers, timeout: Duration): Future[FullResponse] = execute(Request.Head(url, headers), timeout).flatMap{ _.toFullResponse(0) }
  private def getFullImpl(url: String, headers: Headers, maxLength: Long, timeout: Duration): Future[FullResponse] = getAsyncImpl(url, headers, timeout).flatMap{ _.toFullResponse(maxLength) }
  private def getFullStringImpl(url: String, headers: Headers, maxLength: Long, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = getAsyncImpl(url, headers, timeout).flatMap{ _.toFullStringResponse(maxLength, defaultCharset) }
  private def postFullImpl(url: String, body: String, headers: Headers, maxLength: Long, timeout: Duration): Future[FullResponse] = postAsyncImpl(url, body, headers, timeout).flatMap{ _.toFullResponse(maxLength) }
  private def postFullStringImpl(url: String, body: String, headers: Headers, maxLength: Long, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = postAsyncImpl(url, body, headers, timeout).flatMap{ _.toFullStringResponse(maxLength, defaultCharset) }
  private def getAsyncImpl(url: String, headers: Headers, timeout: Duration): Future[AsyncResponse] = execute(Request.Get(url, headers), timeout)
  private def postAsyncImpl(url: String, body: String, headers: Headers, timeout: Duration): Future[AsyncResponse] = execute(Request.Post(url, headers, body), timeout)
  private def postAsyncImpl(url: String, body: Array[Byte], headers: Headers, timeout: Duration): Future[AsyncResponse] = execute(Request.Post(url, headers, body), timeout)
  private def postAsyncImpl(url: String, body: File, headers: Headers, timeout: Duration): Future[AsyncResponse] = execute(Request.Post(url, headers, body), timeout)

  //
  // The rest of the file was auto-generated using the following code (copy/:paste into a REPL):
  //
/*

object Param { def apply(name: String, tpe: String): RequiredParam = RequiredParam(name, tpe); def apply(name: String, tpe: String, default: String): OptionalParam = OptionalParam(name, tpe, default) }
sealed trait Param { def name: String; def tpe: String }
final case class RequiredParam(name: String, tpe: String) extends Param
final case class OptionalParam(name: String, tpe: String, default: String) extends Param

def makeMethodCombinations(name: String, implName: String, returnType: String, requiredParams: IndexedSeq[RequiredParam], optionalParams: IndexedSeq[OptionalParam], isFinal: Boolean = true, indent: String = "  ", comment: String = ""): String = {
	require(name != implName, s"name: $name and implName: $implName should be different")
  val f: String = if (isFinal) "final " else ""
  
  val lines: IndexedSeq[String] = for {
    i: Int <- 0 to optionalParams.size
    optional: IndexedSeq[OptionalParam] <- optionalParams.combinations(i).toVector
  } yield {
    val defParams: String = (requiredParams ++ optional).map{ p: Param => p.name+": "+p.tpe }.mkString(", ")
    val callParams: String = (requiredParams.map{ _.name } ++ optionalParams.map{ p: OptionalParam => if (optional.exists{ _.name == p.name }) p.name else p.default }).mkString(", ")
    s"""${indent}${f}def $name($defParams): $returnType = $implName($callParams)"""
  }
  
  val commentLine: String = s"$indent/** $comment */"
  val withComments: IndexedSeq[String] = if (comment != "") lines.flatMap{ line: String => List(commentLine, line, "") } else lines
  withComments.mkString("\n")
}

println(makeMethodCombinations("head", "headImpl", "Future[FullResponse]", Vector(Param("url", "String")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("timeout", "Duration", "defaultResponseTimeout")), comment = "Perform a HEAD request.  Always returns a FullResponse because the body will be empty"))
println(makeMethodCombinations("getFull", "getFullImpl", "Future[FullResponse]", Vector(Param("url", "String")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("maxLength", "Long", "defaultMaxLength"), Param("timeout", "Duration", "defaultResponseTimeout")), comment = "Perform a GET request returning the FullResponse"))
println(makeMethodCombinations("getFullString", "getFullStringImpl", "Future[FullStringResponse]", Vector(Param("url", "String")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("maxLength", "Long", "defaultMaxLength"), Param("timeout", "Duration", "defaultResponseTimeout"), Param("defaultCharset", "Charset", "defaultCharset")), comment = "Perform a GET request returning the FullStringResponse"))
println(makeMethodCombinations("postFull", "postFullImpl", "Future[FullResponse]", Vector(Param("url", "String"), Param("body", "String")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("maxLength", "Long", "defaultMaxLength"), Param("timeout", "Duration", "defaultResponseTimeout")), comment = "Perform a POST request returning the FullResponse"))
println(makeMethodCombinations("postFullString", "postFullStringImpl", "Future[FullStringResponse]", Vector(Param("url", "String"), Param("body", "String")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("maxLength", "Long", "defaultMaxLength"), Param("timeout", "Duration", "defaultResponseTimeout"), Param("defaultCharset", "Charset", "defaultCharset")), comment = "Perform a POST request returning the FullStringResponse"))
println(makeMethodCombinations("getAsync", "getAsyncImpl", "Future[AsyncResponse]", Vector(Param("url", "String")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("timeout", "Duration", "defaultResponseTimeout")), comment = "Perform a GET request returning an AsyncResponse for reading arbitrarily long response bodies"))
println(makeMethodCombinations("postAsync", "postAsyncImpl", "Future[AsyncResponse]", Vector(Param("url", "String"), Param("body", "Array[Byte]")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("timeout", "Duration", "defaultResponseTimeout")), comment = "Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies"))
println(makeMethodCombinations("postAsync", "postAsyncImpl", "Future[AsyncResponse]", Vector(Param("url", "String"), Param("body", "String")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("timeout", "Duration", "defaultResponseTimeout")), comment = "Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies"))
println(makeMethodCombinations("postAsync", "postAsyncImpl", "Future[AsyncResponse]", Vector(Param("url", "String"), Param("body", "File")), Vector(Param("headers", "Headers", "defaultHeaders"), Param("timeout", "Duration", "defaultResponseTimeout")), comment = "Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies"))

 */
  /** Perform a HEAD request.  Always returns a FullResponse because the body will be empty */
  final def head(url: String): Future[FullResponse] = headImpl(url, defaultHeaders, defaultResponseTimeout)

  /** Perform a HEAD request.  Always returns a FullResponse because the body will be empty */
  final def head(url: String, headers: Headers): Future[FullResponse] = headImpl(url, headers, defaultResponseTimeout)

  /** Perform a HEAD request.  Always returns a FullResponse because the body will be empty */
  final def head(url: String, timeout: Duration): Future[FullResponse] = headImpl(url, defaultHeaders, timeout)

  /** Perform a HEAD request.  Always returns a FullResponse because the body will be empty */
  final def head(url: String, headers: Headers, timeout: Duration): Future[FullResponse] = headImpl(url, headers, timeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String): Future[FullResponse] = getFullImpl(url, defaultHeaders, defaultMaxLength, defaultResponseTimeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String, headers: Headers): Future[FullResponse] = getFullImpl(url, headers, defaultMaxLength, defaultResponseTimeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String, maxLength: Long): Future[FullResponse] = getFullImpl(url, defaultHeaders, maxLength, defaultResponseTimeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String, timeout: Duration): Future[FullResponse] = getFullImpl(url, defaultHeaders, defaultMaxLength, timeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String, headers: Headers, maxLength: Long): Future[FullResponse] = getFullImpl(url, headers, maxLength, defaultResponseTimeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String, headers: Headers, timeout: Duration): Future[FullResponse] = getFullImpl(url, headers, defaultMaxLength, timeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String, maxLength: Long, timeout: Duration): Future[FullResponse] = getFullImpl(url, defaultHeaders, maxLength, timeout)

  /** Perform a GET request returning the FullResponse */
  final def getFull(url: String, headers: Headers, maxLength: Long, timeout: Duration): Future[FullResponse] = getFullImpl(url, headers, maxLength, timeout)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers): Future[FullStringResponse] = getFullStringImpl(url, headers, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, maxLength: Long): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, timeout: Duration): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, defaultMaxLength, timeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers, maxLength: Long): Future[FullStringResponse] = getFullStringImpl(url, headers, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers, timeout: Duration): Future[FullStringResponse] = getFullStringImpl(url, headers, defaultMaxLength, timeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, headers, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, maxLength: Long, timeout: Duration): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, maxLength, timeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, maxLength: Long, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, defaultMaxLength, timeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers, maxLength: Long, timeout: Duration): Future[FullStringResponse] = getFullStringImpl(url, headers, maxLength, timeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers, maxLength: Long, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, headers, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, headers, defaultMaxLength, timeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, maxLength: Long, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, defaultHeaders, maxLength, timeout, defaultCharset)

  /** Perform a GET request returning the FullStringResponse */
  final def getFullString(url: String, headers: Headers, maxLength: Long, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = getFullStringImpl(url, headers, maxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String): Future[FullResponse] = postFullImpl(url, body, defaultHeaders, defaultMaxLength, defaultResponseTimeout)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String, headers: Headers): Future[FullResponse] = postFullImpl(url, body, headers, defaultMaxLength, defaultResponseTimeout)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String, maxLength: Long): Future[FullResponse] = postFullImpl(url, body, defaultHeaders, maxLength, defaultResponseTimeout)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String, timeout: Duration): Future[FullResponse] = postFullImpl(url, body, defaultHeaders, defaultMaxLength, timeout)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String, headers: Headers, maxLength: Long): Future[FullResponse] = postFullImpl(url, body, headers, maxLength, defaultResponseTimeout)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String, headers: Headers, timeout: Duration): Future[FullResponse] = postFullImpl(url, body, headers, defaultMaxLength, timeout)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String, maxLength: Long, timeout: Duration): Future[FullResponse] = postFullImpl(url, body, defaultHeaders, maxLength, timeout)

  /** Perform a POST request returning the FullResponse */
  final def postFull(url: String, body: String, headers: Headers, maxLength: Long, timeout: Duration): Future[FullResponse] = postFullImpl(url, body, headers, maxLength, timeout)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers): Future[FullStringResponse] = postFullStringImpl(url, body, headers, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, maxLength: Long): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, timeout: Duration): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, defaultMaxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers, maxLength: Long): Future[FullStringResponse] = postFullStringImpl(url, body, headers, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers, timeout: Duration): Future[FullStringResponse] = postFullStringImpl(url, body, headers, defaultMaxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, headers, defaultMaxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, maxLength: Long, timeout: Duration): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, maxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, maxLength: Long, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, defaultMaxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers, maxLength: Long, timeout: Duration): Future[FullStringResponse] = postFullStringImpl(url, body, headers, maxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers, maxLength: Long, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, headers, maxLength, defaultResponseTimeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, headers, defaultMaxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, maxLength: Long, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, defaultHeaders, maxLength, timeout, defaultCharset)

  /** Perform a POST request returning the FullStringResponse */
  final def postFullString(url: String, body: String, headers: Headers, maxLength: Long, timeout: Duration, defaultCharset: Charset): Future[FullStringResponse] = postFullStringImpl(url, body, headers, maxLength, timeout, defaultCharset)

  /** Perform a GET request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def getAsync(url: String): Future[AsyncResponse] = getAsyncImpl(url, defaultHeaders, defaultResponseTimeout)

  /** Perform a GET request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def getAsync(url: String, headers: Headers): Future[AsyncResponse] = getAsyncImpl(url, headers, defaultResponseTimeout)

  /** Perform a GET request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def getAsync(url: String, timeout: Duration): Future[AsyncResponse] = getAsyncImpl(url, defaultHeaders, timeout)

  /** Perform a GET request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def getAsync(url: String, headers: Headers, timeout: Duration): Future[AsyncResponse] = getAsyncImpl(url, headers, timeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: Array[Byte]): Future[AsyncResponse] = postAsyncImpl(url, body, defaultHeaders, defaultResponseTimeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: Array[Byte], headers: Headers): Future[AsyncResponse] = postAsyncImpl(url, body, headers, defaultResponseTimeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: Array[Byte], timeout: Duration): Future[AsyncResponse] = postAsyncImpl(url, body, defaultHeaders, timeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: Array[Byte], headers: Headers, timeout: Duration): Future[AsyncResponse] = postAsyncImpl(url, body, headers, timeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: String): Future[AsyncResponse] = postAsyncImpl(url, body, defaultHeaders, defaultResponseTimeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: String, headers: Headers): Future[AsyncResponse] = postAsyncImpl(url, body, headers, defaultResponseTimeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: String, timeout: Duration): Future[AsyncResponse] = postAsyncImpl(url, body, defaultHeaders, timeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: String, headers: Headers, timeout: Duration): Future[AsyncResponse] = postAsyncImpl(url, body, headers, timeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: File): Future[AsyncResponse] = postAsyncImpl(url, body, defaultHeaders, defaultResponseTimeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: File, headers: Headers): Future[AsyncResponse] = postAsyncImpl(url, body, headers, defaultResponseTimeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: File, timeout: Duration): Future[AsyncResponse] = postAsyncImpl(url, body, defaultHeaders, timeout)

  /** Perform a POST request returning an AsyncResponse for reading arbitrarily long response bodies */
  final def postAsync(url: String, body: File, headers: Headers, timeout: Duration): Future[AsyncResponse] = postAsyncImpl(url, body, headers, timeout)

}
