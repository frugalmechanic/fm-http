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

import fm.common.{ImmutableDate, Logging, TestHelpers}
import fm.common.Implicits._
import fm.lazyseq.LazySeq
import io.netty.buffer.{ByteBuf, Unpooled}
import java.io.{File, RandomAccessFile}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._

object TestClientAndServer extends Logging {
  val port: Int = 1234
  val requestCount: Int = 1000

  private val tmpFile: File = {
    val f: File = makeTempFile("This is a temp file")
    f.deleteOnExit() // This is okay since it's only a single file and a short lived process
    f
  }

  private def makeRandomAccessFile(contents: String): RandomAccessFile = {
    val f: File = makeTempFile(contents)
    val raf: RandomAccessFile = new RandomAccessFile(f, "r")
    f.delete() // Our RandomAccessFile is open so should be safe to delete from the file system
    raf
  }

  private def makeTempFile(contents: String): File = {
    val f: File = File.createTempFile("fm-http-tests", ".tmp")
    Files.write(f.toPath, contents.getBytes(StandardCharsets.UTF_8))
    f
  }
  
  import fm.http.client.HttpClient
  import fm.http.server._

  val client: HttpClient = HttpClient(maxConnectionsPerHost = 1000, maxRequestQueuePerHost = requestCount, defaultResponseTimeout = 60.seconds)
  val clientNoFollowRedirects: HttpClient = HttpClient(maxConnectionsPerHost = 1000, maxRequestQueuePerHost = requestCount, defaultResponseTimeout = 60.seconds, followRedirects = false)
  val clientWithExpect100Continue: HttpClient = HttpClient(maxConnectionsPerHost = 1000, maxRequestQueuePerHost = requestCount, defaultResponseTimeout = 60.seconds, useExpect100Continue = true)

  def startServer(): Unit = server
  def stopServer(): Unit = server.shutdown()

  private val options: HttpServerOptions = HttpServerOptions.default
    .withRequestIdResponseHeader(Some("X-Request-Id"))
    .withClientIPLookupSpecs(Seq(
      HttpServerOptions.ClientIPLookupSpec(
        headerName = "X-Client-IP",
        requiredHeaderAndValue = Some(("X-Is-From-CDN", "abc123")),
        valueToUse = HttpServerOptions.ClientIPHeaderValueToUse.First
      ),
      HttpServerOptions.defaultClientIPLookupSpec,
      HttpServerOptions.ClientIPLookupSpec(
        headerName = "X-Forwarded-For-First-Idx0",
        requiredHeaderAndValue = None,
        valueToUse = HttpServerOptions.ClientIPHeaderValueToUse.OffsetFromFirst(0)
      ),
      HttpServerOptions.ClientIPLookupSpec(
        headerName = "X-Forwarded-For-First-Idx1",
        requiredHeaderAndValue = None,
        valueToUse = HttpServerOptions.ClientIPHeaderValueToUse.OffsetFromFirst(1)
      ),
      HttpServerOptions.ClientIPLookupSpec(
        headerName = "X-Forwarded-For-First-Idx2",
        requiredHeaderAndValue = None,
        valueToUse = HttpServerOptions.ClientIPHeaderValueToUse.OffsetFromFirst(2)
      ),
      HttpServerOptions.ClientIPLookupSpec(
        headerName = "X-Forwarded-For-Last-Idx0",
        requiredHeaderAndValue = None,
        valueToUse = HttpServerOptions.ClientIPHeaderValueToUse.OffsetFromLast(0)
      ),
      HttpServerOptions.ClientIPLookupSpec(
        headerName = "X-Forwarded-For-Last-Idx1",
        requiredHeaderAndValue = None,
        valueToUse = HttpServerOptions.ClientIPHeaderValueToUse.OffsetFromLast(1)
      ),
      HttpServerOptions.ClientIPLookupSpec(
        headerName = "X-Forwarded-For-Last-Idx2",
        requiredHeaderAndValue = None,
        valueToUse = HttpServerOptions.ClientIPHeaderValueToUse.OffsetFromLast(2)
      )
    ))
  
  private lazy val server: HttpServer = HttpServer(port, router, "ABC123", options)
  
  import RouteMatchers._
  
  private def router = RequestRouter(handler)

  private val OneMB: Long = 1048576

  private val UTF8Header: Headers = Headers(("Content-Type", "text/html; charset=utf-8"))

  // Quoting of the utf-8 seems to not be valid and should be ignored.  Previously this threw an exception
  private val QuotedUTF8Header: Headers = Headers(("Content-Type", "text/html; charset=\"utf-8\""))

  private val Latin1Header: Headers = Headers(("Content-Type", "text/html; charset=ISO-8859-1"))

  private val jsonHeader: Headers = Headers(("Content-Type", MimeTypes.JSON))

  private val jsonLatin1Header: Headers = Headers(("Content-Type", s"${MimeTypes.JSON}; charset=ISO-8859-1"))

  private implicit def responseToFugure(r: Response): Future[Response] = Future.successful(r)
  import scala.concurrent.ExecutionContext.Implicits.global

  protected val unwrappedHandler: PartialFunction[Request, Future[Response]] = (request: Request) => request match {
    case GET(simple"/${INT(code)}")                    => Response(Status(code), Status(code).msg)
    case GET(simple"/close/${INT(code)}")              => Response(Status(code), Headers("Connection" -> "close"), Status(code).msg)
    case GET("/data_one_mb")                           => Response.Ok(makeLinkedHttpContent(OneMB))
    case GET("/data_hundred_mb")                       => Response.Ok(makeLinkedHttpContent(OneMB * 100))

    case POST("/write_to_file_max_length_one_mb")      => {
      val file = File.createTempFile("fm-http-tests", ".tmp")

      val res: Future[Response] = request.content.writeToFile(file, OneMB, LinkedHttpContentReader.MaxLengthStrategy.DiscardAndThrowException).map { _ =>
        Response.Ok(file.length.toString)
      }.recoverWith {
        case ex: LinkedHttpContentReader.MaxLengthException => Future.successful(Response.plain(Status.REQUEST_ENTITY_TOO_LARGE, "413 Request Entity Too Large"))
      }

      res.onComplete { _ => file.delete() }

      res
    }

    case POST("/read_to_string_max_length_one_mb")     => {
      request.content.readToString(OneMB, LinkedHttpContentReader.MaxLengthStrategy.DiscardAndThrowException).map { data: String =>
        Response.Ok(data.size.toString)
      }.recoverWith {
        case ex: LinkedHttpContentReader.MaxLengthException => Future.successful(Response.plain(Status.REQUEST_ENTITY_TOO_LARGE, "413 Request Entity Too Large"))
      }
    }

    case POST("/read_to_byte_array_max_length_one_mb") => {
      request.content.readToByteArray(OneMB, LinkedHttpContentReader.MaxLengthStrategy.DiscardAndThrowException).map { data: Array[Byte] =>
        Response.Ok(data.size.toString)
      }.recoverWith {
        case ex: LinkedHttpContentReader.MaxLengthException => Future.successful(Response.plain(Status.REQUEST_ENTITY_TOO_LARGE, "413 Request Entity Too Large"))
      }
    }

    case GET("/utf-8")                                 => Response.Ok(UTF8Header, Unpooled.copiedBuffer("£", StandardCharsets.UTF_8))
    case GET("/json")                                  => Response.Ok(jsonHeader, Unpooled.copiedBuffer("£", StandardCharsets.UTF_8))
    case GET("/json-latin1")                           => Response.Ok(jsonLatin1Header, Unpooled.copiedBuffer("£", StandardCharsets.ISO_8859_1))
    case GET("/json-latin1-utf8-data")                 => Response.Ok(jsonLatin1Header, Unpooled.copiedBuffer("£", StandardCharsets.UTF_8))

    case GET("/latin1")                                => Response.Ok(Latin1Header, Unpooled.copiedBuffer("£", StandardCharsets.ISO_8859_1))
    case GET("/default-latin1")                        => Response.Ok(Headers.empty, Unpooled.copiedBuffer("£", StandardCharsets.ISO_8859_1))
    case GET("/latin1-header-utf8-data")               => Response.Ok(Latin1Header, Unpooled.copiedBuffer("£", StandardCharsets.UTF_8))

    case GET("/bad_content_type_charset")              => Response.plain(Status.OK, QuotedUTF8Header, "Hello World")

    case GET("/ok")                                    => Response.Ok(Headers.empty, Unpooled.copiedBuffer("ok", StandardCharsets.ISO_8859_1))
    case GET("/redirect")                              => Response.Found("/ok")
    
    case GET("/redirect1")                             => Response.Found("/redirect")
    case GET("/redirect2")                             => Response.MovedPermanently("/redirect1")
    case GET("/redirect3")                             => Response.Found(s"http://localhost:$port/redirect2")
    case GET("/redirect4")                             => Response.Found("/redirect3")
    case GET("/redirect5")                             => Response.Found("/redirect4")
    case GET("/redirect6")                             => Response.Found("/redirect5")

    case GET("/ip")                                    => Response.Ok(request.remoteIp.toString)

    case GET("/basic_auth")                            => handleBasicAuth(request)

    case GET("/file")                                  => Response.Ok(UTF8Header, tmpFile)
    case GET("/random_access_file")                    => Response.Ok(UTF8Header, makeRandomAccessFile("This is a random access file"))

    case GET("/header_modifications")                  => {
      implicit val r: Request = request

      // This should override what gets set as part of the Response.Ok line
      Response.setHeader("foo", "bar")

      // Should be in addition to the header below
      Response.addHeader("additive", "two")

      // Should remove the "remove_me" header that gets set below
      Response.removeHeader("remove_me")

      Response.modifyHeaders{ headers: MutableHeaders =>
        headers.add("Hello", "World")
        headers.date = ImmutableDate(1234567890000L)
      }

      val headers: Headers = Headers(
        "foo" -> "not_bar",
        "additive" -> "one",
        "remove_me" -> "asd"
      )

      Response.Ok(headers, Unpooled.copiedBuffer("ok", StandardCharsets.ISO_8859_1))
    }

    case POST("/upload")                               => request.content.foldLeft(0){ (sum,buf) => sum + buf.readableBytes() }.map{ sum: Int => Response.Ok(sum.toString) }
    case POST("/close/upload")                         => request.content.foldLeft(0){ (sum,buf) => sum + buf.readableBytes() }.map{ sum: Int => Response(Status(200), Headers("Connection" -> "close"), sum.toString) }
  }

  private def handleBasicAuth(request: Request): Response = {
    if (request.headers.basicAuthUserAndPass === Some(("foo", "bar"))) Response.Ok(Headers.empty, Unpooled.copiedBuffer("ok", StandardCharsets.ISO_8859_1))
    else Response(Status.UNAUTHORIZED, Headers("WWW-Authenticate" -> """Basic realm="Test""""), Unpooled.copiedBuffer("You need a valid user and password to access this content.", StandardCharsets.ISO_8859_1))
  }
  
  // This just cycles through all the ASCII printable chars starting at the space ' ' (20) and ending with '~' (126)
  private val MaxChars: Int = '~' - ' '
  
  def charForIdx(idx: Int): Char = charForIdx(idx.toLong)
  def charForIdx(idx: Long): Char = (idx % MaxChars + ' ').toChar
  
  private val BodyChunkSize: Int = 2048
  private val BodyBuf: Array[Byte] = {
    val b = Array.newBuilder[Byte]
    
    var i = 0
    
    while(i < BodyChunkSize + MaxChars) {
      b += charForIdx(i).toByte
      i += 1
    }
    
    b.result()
  }

  protected def makeLinkedHttpContent(sizeBytes: Long): LinkedHttpContent = makeLinkedHttpContent(sizeBytes, 0)

  protected def makeLinkedHttpContent(sizeBytes: Long, idx: Long): LinkedHttpContent = {
    if (sizeBytes <= 0) return LinkedHttpContent(Unpooled.EMPTY_BUFFER)
    
    val sizeToGenerate: Int = math.min(sizeBytes, BodyChunkSize).toInt
    
    val buf: ByteBuf = Unpooled.wrappedBuffer(BodyBuf, (idx % MaxChars).toInt, sizeToGenerate)
    
    require(buf.readableBytes() == sizeToGenerate)
    LinkedHttpContent.async(buf, Future.successful(Some(makeLinkedHttpContent(sizeBytes - sizeToGenerate, idx + sizeToGenerate))))
  }
  
  protected val handler: PartialFunction[Request, Future[Response]] = new PartialFunction[Request, Future[Response]] {
    def apply(request: Request): Future[Response] = wrap(request, unwrappedHandler(request))
    def isDefinedAt(request: Request): Boolean = unwrappedHandler.isDefinedAt(request)
  }
  
  def wrap(request: Request, f: => Future[Response]): Future[Response] = {
    request.params.getFirstNonBlank("delay").flatMap{ _.toIntOption } match {
      case Some(delay) =>
        val p = Promise[Response]()
        server.timer.schedule(delay.seconds){ p.completeWith(f) }
        p.future
      case None => f
    }
  }
}

// TODO: split this into a "stress test" mode and a "normal" unit testing mode
final class TestClientAndServer extends FunSuite with Matchers with BeforeAndAfterAll with Logging {
  import TestClientAndServer.{charForIdx, client, clientNoFollowRedirects, port, requestCount}
  import client.executionContext
  import fm.http.client._
  import TestClientAndServer.OneMB

  override def beforeAll(): Unit = {
    Logging.setLevelToWarn(classOf[HttpClient])
    TestClientAndServer.startServer()
  }
  
  override def afterAll(): Unit = {
    Logging.setLevelToInfo(classOf[HttpClient])
    TestClientAndServer.stopServer()
  }
    
  private def makeUrl(path: String): String = {
    require(path.startsWith("/"), "Path must start with a slash: "+path)
    s"http://127.0.0.1:${TestClientAndServer.port}"+path
  }

  private def getSync(
    path: String,
    expectedCode: Int,
    expectedBody: String
  ): FullStringResponse = getSync(path, expectedCode, expectedBody, client)

  private def getSync(
    path: String,
    expectedCode: Int,
    expectedBody: String,
    httpClient: HttpClient,
  ): FullStringResponse = getSync(path, expectedCode, expectedBody, httpClient, Headers.empty)

  private def getSync(
    path: String,
    expectedCode: Int,
    expectedBody: String,
    httpClient: HttpClient,
    headers: Headers,
  ): FullStringResponse = TestHelpers.withCallerInfo{
    val f: Future[FullStringResponse] = getFullStringAsync(path, httpClient, headers)
    val res: FullStringResponse = Await.result(f, 10.seconds)
    res.status.code should equal (expectedCode)
    res.body should equal (expectedBody)
    res
  }

  private def postSync(
    path: String,
    postBody: String,
    expectedCode: Int,
    expectedBody: String
  ): Unit = postSync(path, postBody, expectedCode, expectedBody, client)

  private def postSync(
    path: String,
    postBody: String,
    expectedCode: Int,
    expectedBody: String,
    httpClient: HttpClient
  ): Unit = TestHelpers.withCallerInfo{
    val f: Future[FullStringResponse] = postFullStringAsync(path, postBody, httpClient)
    val res: FullStringResponse = Await.result(f, 10.seconds)
    res.status.code should equal (expectedCode)
    res.body should equal (expectedBody)
  }
  
  private def getFullStringAsync(
    path: String,
    httpClient: HttpClient = client,
    headers: Headers = Headers.empty
  ): Future[FullStringResponse] = {
    httpClient.getFullString(makeUrl(path), headers)
  }

  private def postFullStringAsync(
    path: String,
    postBody: String,
  ): Future[FullStringResponse] = postFullStringAsync(path, postBody, client)

  private def postFullStringAsync(
    path: String,
    postBody: String,
    httpClient: HttpClient,
  ): Future[FullStringResponse] = {
    httpClient.postFullString(makeUrl(path), postBody)
  }
  
  private def getAndVerifyData(path: String): Future[Boolean] = {
    client.getAsync(makeUrl(path)).flatMap{ response: AsyncResponse =>
      response.status.code should equal (200)
      response.body.get.foldLeft(0L){ (idx, buf) =>
        var i: Int = 0
        while(i < buf.readableBytes()) {
          val char: Char = buf.getByte(i).toChar
          val expected: Char = charForIdx(idx + i)
          
          if (char != expected) char should equal (expected)
          
          // This is slooooow:
          //buf.getByte(i).toChar should equal (charForIdx(idx + i))
          i += 1
        }
        
        idx + i
      }.map{ _ => true }
    }
  }

  test("Single Request") {
    getSync("/200", 200, "OK")
  }
  
  test("Multiple Sequential Requests (1,000 Requests)") {
    (1 to 1000).foreach { _ => getSync("/200", 200, "OK") }
  }
  
  test(s"Parallel Sync GET Requests ($requestCount Requests)") {
    // Uses blocking calls to maintain a constant number of connections to the server
    LazySeq.wrap(1 to requestCount).parForeach(threads=64){ _ =>
      getSync("/200", 200, "OK")
    }
  }

  test("LinkedHttpContentReader.writeToFile") {


    checkMaxLength("/write_to_file_max_length_one_mb", OneMB, Status.OK, (OneMB).toString)
    checkMaxLength("/write_to_file_max_length_one_mb", OneMB/2, Status.OK, (OneMB/2).toString)
    checkMaxLength("/write_to_file_max_length_one_mb", OneMB*2, Status.REQUEST_ENTITY_TOO_LARGE, "413 Request Entity Too Large")
  }

  test("LinkedHttpContentReader.readToString") {
    checkMaxLength("/read_to_string_max_length_one_mb", OneMB, Status.OK, OneMB.toString)
    checkMaxLength("/read_to_string_max_length_one_mb", OneMB/2, Status.OK, (OneMB/2).toString)
    checkMaxLength("/read_to_string_max_length_one_mb", OneMB*2, Status.REQUEST_ENTITY_TOO_LARGE, "413 Request Entity Too Large")

    // Real data is less than correct size, but verify the content-length checking is working correct.
    checkMaxLength(
      "/read_to_string_max_length_one_mb",
       OneMB/2, Status.REQUEST_ENTITY_TOO_LARGE,
      "413 Request Entity Too Large",
      client.defaultHeaders.withHeaders(("Content-Length", OneMB*2)),
      TestClientAndServer.clientWithExpect100Continue
    )
  }

  test("LinkedHttpContentReader.readToByteArray") {
    checkMaxLength("/read_to_byte_array_max_length_one_mb", OneMB, Status.OK, OneMB.toString)
    checkMaxLength("/read_to_byte_array_max_length_one_mb", OneMB*2, Status.REQUEST_ENTITY_TOO_LARGE, "413 Request Entity Too Large")
  }

  private def checkMaxLength(path: String, size: Long, expectedStatus: Status, expectedBody: String, headers: Headers = client.defaultHeaders, httpClient: HttpClient = client): Unit = {
    val res: AsyncResponse = Await.result(postAsyncSize(path, size, headers, httpClient), 60.seconds)

    res.status should equal(expectedStatus)
    Await.result(res.readBodyToString(), 10.seconds) should equal(expectedBody)
  }

  private def postAsyncSize(path: String, size: Long, headers: Headers, httpClient: HttpClient): Future[AsyncResponse] = {
    val content: LinkedHttpContent = TestClientAndServer.makeLinkedHttpContent(size)
    httpClient.postAsync(makeUrl(path), content, headers)
  }

  test(s"Parallel Sync POST Requests ($requestCount Requests)") {
    // Uses blocking calls to maintain a constant number of connections to the server
    LazySeq.wrap(1 to requestCount).parForeach(threads=64){ _ =>
      val body: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"
      postSync("/upload", body, 200, body.length.toString)
    }
  }
  
  test(s"Async Requests ($requestCount Requests)") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val futures: Seq[Future[FullStringResponse]] = (1 to requestCount).map{ _ => getFullStringAsync("/200") }
    val combined: Future[Seq[FullStringResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullStringResponse =>
      res.status.code should equal (200)
      res.body should equal ("OK")
    }
  }

  test(s"Async POST Requests ($requestCount Requests)") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val body: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"
    val futures: Seq[Future[FullStringResponse]] = (1 to requestCount).map{ _ => postFullStringAsync("/upload", body) }
    val combined: Future[Seq[FullStringResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullStringResponse =>
      res.status.code should equal (200)
      res.body should equal (body.length.toString)
    }
  }
  
  test(s"Async Requests ($requestCount Requests) - Connection: close") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val futures: Seq[Future[FullStringResponse]] = (1 to requestCount).map{ _ => getFullStringAsync("/close/200") }
    val combined: Future[Seq[FullStringResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullStringResponse =>
      res.status.code should equal (200)
      res.body should equal ("OK")
    }
  }

  test(s"Async Requests POST ($requestCount Requests) - Connection: close") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val body: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"
    val futures: Seq[Future[FullStringResponse]] = (1 to requestCount).map{ _ => postFullStringAsync("/close/upload", body) }
    val combined: Future[Seq[FullStringResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullStringResponse =>
      res.status.code should equal (200)
      res.body should equal (body.length.toString)
    }
  }
  
  test(s"Async Requests with delayed response ($requestCount Requests)") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val futures: Seq[Future[FullStringResponse]] = (1 to requestCount).map{ _ => getFullStringAsync("/200?delay=1") }
    val combined: Future[Seq[FullStringResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullStringResponse =>
      res.status.code should equal (200)
      res.body should equal ("OK")
    }
  }
  
  test(s"Async Requests with delayed response ($requestCount Requests) - Connection: close") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val futures: Seq[Future[FullStringResponse]] = (1 to requestCount).map{ _ => getFullStringAsync("/close/200?delay=1") }
    val combined: Future[Seq[FullStringResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullStringResponse =>
      res.status.code should equal (200)
      res.body should equal ("OK")
    }
  }
  
  test("Single Request with Large Response Body (1 MB)") {
    Await.result(getAndVerifyData("/data_one_mb"), 10.seconds)
  }
  
  test("Single Request with Large Response Body (100 MB)") {
    Await.result(getAndVerifyData("/data_hundred_mb"), 60.seconds)
  }
  
  test("Parallel Sync Requests with Large Response Body (200 Requests, 1 MB)") {
    // Uses blocking calls to maintain a constant number of connections to the server
    LazySeq.wrap(1 to 200).parForeach(threads=64){ _ =>
      Await.result(getAndVerifyData("/data_one_mb"), 10.seconds)
    }
  }

  test("Single Request with Large Response Body (100 MB) with MaxLength 10MB") {
    Await.result(getAndVerifyData("/data_hundred_mb"), 60.seconds)
  }


  //  // This test uses up too much native memory:
//  test("Async Requests with Large Response Body (1,000 Requests, 1 MB)") {
//    // Uses async non-blocking calls to make as many connections as possible to the server
//    val futures: Seq[Future[Boolean]] = (1 to 1000).map{ _ => getAndVerifyData("/data_one_mb") }
//    val combined: Future[Seq[Boolean]] = Future.sequence(futures)
//    Await.result(combined, 60.seconds).foreach { res: Boolean =>
//      res should equal (true)
//    }
//  }

  /*
  Certain characters like £ are two bytes in UTF-8 and one byte in Latin1
  Test support for reading explicit latin1/utf-8/etc response bodies

  scala> "£".getBytes("UTF-8").map { _ & 0xFF }
  res10: Array[Int] = Array(194, 163)

  scala> "£".getBytes("latin1").map { _ & 0xFF }
  res11: Array[Int] = Array(163)
   */

  test("Content-Type: UTF-8") {
    getSync("/utf-8", 200, "£")
  }

  test("Content-Type: latin1") {
    getSync("/latin1", 200, "£")
  }

  test("Content-Type: default latin1") {
    getSync("/default-latin1", 200, "£")
  }

  test("Content-Type: latin1 & UTF-8 Data") {
    getSync("/latin1-header-utf8-data", 200, "Â£") // new String("£".getBytes(java.nio.charset.StandardCharsets.UTF_8), java.nio.charset.StandardCharsets.ISO_8859_1)
  }

  test(s"Content-Type: ${MimeTypes.JSON} latin1") {
    getSync("/json-latin1", 200, "£")
  }

  test(s"Content-Type: ${MimeTypes.JSON} latin1 & UTF-8 Data") {
    getSync("/json-latin1-utf8-data", 200, "Â£") // new String("£".getBytes(java.nio.charset.StandardCharsets.UTF_8), java.nio.charset.StandardCharsets.ISO_8859_1)
  }

  test("bad_content_type_charset") {
    getSync("/bad_content_type_charset", 200, "Hello World")
  }

  test("Ok") {
    getSync("/ok", 200, "ok")
  }

  test("Redirect") {
    getSync("/redirect", 200, "ok")
  }
  
  test("Redirect 1") {
    getSync("/redirect1", 200, "ok")
  }
  
  test("Redirect 2") {
    getSync("/redirect2", 200, "ok")
  }
  
  test("Redirect 3") {
    getSync("/redirect3", 200, "ok")
  }
  
  test("Redirect 4") {
    getSync("/redirect4", 200, "ok")
  }
  
  test("Redirect 5") {
    intercept[TooManyRedirectsException] { getSync("/redirect5", 200, "ok") }
  }
  
  test("Redirect 6") {
    intercept[TooManyRedirectsException] { getSync("/redirect6", 200, "ok") }
  }
  
  test("Redirect - noFollowRedirects") {
    getSync("/redirect", 302, "/ok", clientNoFollowRedirects)
  }
  
  test("Redirect 1 - noFollowRedirects") {
    getSync("/redirect1", 302, "/redirect", clientNoFollowRedirects)
  }
  
  test("Redirect 2 - noFollowRedirects") {
    getSync("/redirect2", 301, "/redirect1", clientNoFollowRedirects)
  }
  
  test("Redirect 3 - noFollowRedirects") {
    getSync("/redirect3", 302, s"http://localhost:$port/redirect2", clientNoFollowRedirects)
  }
  
  test("Redirect 4 - noFollowRedirects") {
    getSync("/redirect4", 302, "/redirect3", clientNoFollowRedirects)
  }
  
  test("Redirect 5 - noFollowRedirects") {
    getSync("/redirect5", 302, "/redirect4", clientNoFollowRedirects)
  }
  
  test("Redirect 6 - noFollowRedirects") {
    getSync("/redirect6", 302, "/redirect5", clientNoFollowRedirects)
  }

  test("Authentication - Basic Auth") {
    getSync("/basic_auth", 401, "You need a valid user and password to access this content.")
    getSync("/basic_auth", 401, "You need a valid user and password to access this content.", client.withBasicAuth("bar", "foo"))
    getSync("/basic_auth", 200, "ok", client.withBasicAuth("foo", "bar"))
  }

  test("File - Single") {
    getSync("/file", 200, "This is a temp file")
  }

  test("File - 200 Requests") {
    // Uses blocking calls to maintain a constant number of connections to the server
    LazySeq.wrap(1 to 200).parForeach(threads=64){ _ =>
      getSync("/file", 200, "This is a temp file")
    }
  }

  test("RandomAccessFile - Single") {
    getSync("/random_access_file", 200, "This is a random access file")
  }

  test("RandomAccessFile - 200 Requests") {
    // Uses blocking calls to maintain a constant number of connections to the server
    LazySeq.wrap(1 to 200).parForeach(threads=64){ _ =>
      getSync("/random_access_file", 200, "This is a random access file")
    }
  }

  test("Header Modifications") {
    val response: Response = getSync("/header_modifications", 200, "ok")

    response.headers.get("foo") should equal (Some("bar"))
    response.headers.getAll("additive") should equal (Vector("one","two"))
    response.headers.get("remove_me") should equal (None)

    response.headers.get("Hello") should equal (Some("World"))
    response.headers.date should equal (Some(ImmutableDate(1234567890000L)))
  }

  test("Remote IP") {
    checkIp("127.0.0.1")

    checkIp("127.0.0.1", "X-Forwarded-For" -> "")
    checkIp("127.0.0.1", "X-Forwarded-For" -> "foo")
    checkIp("1.1.1.1", "X-Forwarded-For" -> "1.1.1.1")
    checkIp("2.2.2.2", "X-Forwarded-For" -> "1.1.1.1,2.2.2.2")

    checkIp("127.0.0.1", "X-Client-Ip" -> "1.1.1.1")
    checkIp("9.9.9.9", "X-Client-Ip" -> "1.1.1.1", "X-Forwarded-For" -> "9.9.9.9")
    checkIp("9.9.9.9", "X-Client-Ip" -> "foo", "X-Forwarded-For" -> "9.9.9.9")
    checkIp("1.1.1.1", "X-Client-Ip" -> "1.1.1.1", "X-Is-From-CDN" -> "abc123")
    checkIp("127.0.0.1", "X-Client-Ip" -> "foo", "X-Is-From-CDN" -> "abc123")
    checkIp("1.1.1.1", "X-Client-Ip" -> "1.1.1.1", "X-Is-From-CDN" -> "abc123", "X-Forwarded-For" -> "9.9.9.9")
    checkIp("9.9.9.9", "X-Client-Ip" -> "", "X-Is-From-CDN" -> "abc123", "X-Forwarded-For" -> "9.9.9.9")
    checkIp("9.9.9.9", "X-Client-Ip" -> "foo", "X-Is-From-CDN" -> "abc123", "X-Forwarded-For" -> "9.9.9.9")

    checkIp("127.0.0.1", "X-Forwarded-For-First-Idx0" -> "")
    checkIp("127.0.0.1", "X-Forwarded-For-First-Idx0" -> "foo")
    checkIp("1.1.1.1", "X-Forwarded-For-First-Idx0" -> "1.1.1.1")
    checkIp("1.1.1.1", "X-Forwarded-For-First-Idx0" -> "1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4")

    checkIp("127.0.0.1", "X-Forwarded-For-First-Idx1" -> "")
    checkIp("127.0.0.1", "X-Forwarded-For-First-Idx1" -> "foo")
    checkIp("1.1.1.1", "X-Forwarded-For-First-Idx1" -> "1.1.1.1")
    checkIp("2.2.2.2", "X-Forwarded-For-First-Idx1" -> "1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4")

    checkIp("127.0.0.1", "X-Forwarded-For-First-Idx2" -> "")
    checkIp("127.0.0.1", "X-Forwarded-For-First-Idx2" -> "foo")
    checkIp("1.1.1.1", "X-Forwarded-For-First-Idx2" -> "1.1.1.1")
    checkIp("3.3.3.3", "X-Forwarded-For-First-Idx2" -> "1.1.1.1,2.2.2.2,3.3.3.3")
    checkIp("3.3.3.3", "X-Forwarded-For-First-Idx2" -> "1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4")

    checkIp("127.0.0.1", "X-Forwarded-For-Last-Idx0" -> "")
    checkIp("127.0.0.1", "X-Forwarded-For-Last-Idx0" -> "foo")
    checkIp("4.4.4.4", "X-Forwarded-For-Last-Idx0" -> "1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4")
    checkIp("1.1.1.1", "X-Forwarded-For-Last-Idx0" -> "1.1.1.1")

    checkIp("127.0.0.1", "X-Forwarded-For-Last-Idx1" -> "")
    checkIp("127.0.0.1", "X-Forwarded-For-Last-Idx1" -> "foo")
    checkIp("3.3.3.3", "X-Forwarded-For-Last-Idx1" -> "1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4")
    checkIp("1.1.1.1", "X-Forwarded-For-Last-Idx1" -> "1.1.1.1")

    checkIp("127.0.0.1", "X-Forwarded-For-Last-Idx2" -> "")
    checkIp("127.0.0.1", "X-Forwarded-For-Last-Idx2" -> "foo")
    checkIp("1.1.1.1", "X-Forwarded-For-Last-Idx2" -> "1.1.1.1,2.2.2.2,3.3.3.3")
    checkIp("2.2.2.2", "X-Forwarded-For-Last-Idx2" -> "1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4")
    checkIp("1.1.1.1", "X-Forwarded-For-Last-Idx2" -> "1.1.1.1")
  }

  private def checkIp(expected: String, headers: (String,String)*): Unit = TestHelpers.withCallerInfo{
    getSync("/ip", 200, expected, client, headers = Headers(headers:_*))
  }
}
