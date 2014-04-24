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

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.util.CharsetUtil
import fm.common.{Logging, TestHelpers}
import fm.common.Implicits._
import fm.lazyseq.LazySeq

object TestClientAndServer {
  val port: Int = 1234
  val requestCount: Int = 1000
  
  import fm.http.client.HttpClient
  import fm.http.server._
  
  val client: HttpClient = HttpClient(maxConnectionsPerHost = 1000, defaultResponseTimeout = 60.seconds)
  
  def startServer(): Unit = server
  def stopServer(): Unit = server.shutdown()
  
  private lazy val server: HttpServer = HttpServer(port, router, "ABC123")
  
  import RouteMatchers._
  
  private def router = RequestRouter(handler)
  
  private val OneMB: Long = 1048576
  
  protected val unwrappedHandler: PartialFunction[Request, Response] = {
    case GET(simple"/${INT(code)}") => Response(Status(code), Status(code).msg)
    case GET("/data_one_mb")        => Response.Ok(makeLinkedHttpContent(OneMB))
    case GET("/data_hundred_mb")    => Response.Ok(makeLinkedHttpContent(OneMB * 100))
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
  
  protected def makeLinkedHttpContent(sizeBytes: Long, idx: Long = 0): LinkedHttpContent = {
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
  
  def wrap(request: Request, f: => Response): Future[Response] = {
    request.params.getFirstNonBlank("delay").flatMap{ _.toIntOption } match {
      case Some(delay) =>
        val p = Promise[Response]()
        server.timer.schedule(delay.seconds){ p.success(f) }
        p.future
      case None        => Future.successful(f) 
    }
  }  
}

// TODO: split this into a "stress test" mode and a "normal" unit testing mode
final class TestClientAndServer extends FunSuite with Matchers with BeforeAndAfterAll {
  import TestClientAndServer.{charForIdx, client, requestCount}
  import client.executionContext
  import fm.http.client._
  
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
  
  private def getSync(path: String, expectedCode: Int, expectedBody: String): Unit = TestHelpers.withCallerInfo{
    val f: Future[FullResponse] = getFullAsync(path)
    val res: FullResponse = Await.result(f, 5.seconds)
    res.status.code should equal (expectedCode)
    res.body should equal (expectedBody)
  }
  
  private def getFullAsync(path: String): Future[FullResponse] = client.getFull(makeUrl(path))
  
  private def getAndVerifyData(path: String): Future[Boolean] = {
    client.getAsync(makeUrl(path)).flatMap{ response: AsyncResponse =>
      response.status.code should equal (200)
      response.body.get.foldLeft(0L){ (idx, buf) =>
        var i = 0
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
  
  test(s"Parallel Sync Requests ($requestCount Requests)") {
    // Uses blocking calls to maintain a constant number of connections to the server
    LazySeq.wrap(1 to requestCount).parForeach(threads=64){ _ =>
      getSync("/200", 200, "OK")
    }
  }
  
  test(s"Async Requests ($requestCount Requests)") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val futures: Seq[Future[FullResponse]] = (1 to requestCount).map{ _ => getFullAsync("/200") }
    val combined: Future[Seq[FullResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullResponse =>
      res.status.code should equal (200)
      res.body should equal ("OK")
    }
  }
  
  test(s"Async Requests with delayed response ($requestCount Requests)") {
    // Uses async non-blocking calls to make as many connections as possible to the server
    val futures: Seq[Future[FullResponse]] = (1 to requestCount).map{ _ => getFullAsync("/200?delay=1") }
    val combined: Future[Seq[FullResponse]] = Future.sequence(futures)
    Await.result(combined, 60.seconds).foreach { res: FullResponse =>
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
  
//  // This test uses up too much native memory:
//  test("Async Requests with Large Response Body (1,000 Requests, 1 MB)") {
//    // Uses async non-blocking calls to make as many connections as possible to the server
//    val futures: Seq[Future[Boolean]] = (1 to 1000).map{ _ => getAndVerifyData("/data_one_mb") }
//    val combined: Future[Seq[Boolean]] = Future.sequence(futures)
//    Await.result(combined, 60.seconds).foreach { res: Boolean =>
//      res should equal (true)
//    }
//  }
}
