package fm.http.client

import fm.http.Headers
import scala.concurrent.Future
import scala.concurrent.duration.Duration

abstract class HttpClientProxy extends HttpClient {
  protected def client: HttpClient
  
  def defaultMaxLength: Long = client.defaultMaxLength
  def defaultHeaders: Headers = client.defaultHeaders
  def defaultResponseTimeout: Duration = client.defaultResponseTimeout
  
  def execute(r: Request, timeout: Duration): Future[AsyncResponse] = client.execute(r, timeout)
  
  def close(): Unit = client.close()
}