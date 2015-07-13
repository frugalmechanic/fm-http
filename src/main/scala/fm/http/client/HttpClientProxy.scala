package fm.http.client

import fm.http.Headers
import java.nio.charset.Charset
import scala.concurrent.Future
import scala.concurrent.duration.Duration

abstract class HttpClientProxy extends HttpClient {
  protected def client: HttpClient
  
  def defaultMaxLength: Long = client.defaultMaxLength
  def defaultHeaders: Headers = client.defaultHeaders
  def defaultResponseTimeout: Duration = client.defaultResponseTimeout
  def defaultCharset: Charset = client.defaultCharset
  
  def execute(r: Request, timeout: Duration): Future[AsyncResponse] = client.execute(r, timeout)
  
  def close(): Unit = client.close()
}