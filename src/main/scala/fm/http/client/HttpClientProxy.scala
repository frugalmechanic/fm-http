package fm.http.client

import fm.http.Headers
import java.nio.charset.Charset
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

abstract class HttpClientProxy extends HttpClient {
  protected def client: HttpClient

  override implicit def executionContext: ExecutionContext = client.executionContext

  override def defaultMaxLength: Long = client.defaultMaxLength
  override def defaultHeaders: Headers = client.defaultHeaders
  override def defaultResponseTimeout: Duration = client.defaultResponseTimeout
  override def defaultCharset: Charset = client.defaultCharset

  override def execute(r: Request, timeout: Duration): Future[AsyncResponse] = client.execute(r, timeout)

  override def close(): Unit = client.close()
}