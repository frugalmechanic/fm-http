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

  override def loggingHooksEnabled: Boolean = client.loggingHooksEnabled
  override def logSuccess(request: Request, requestBody: Option[String], response: Response): Unit = client.logSuccess(request, requestBody, response)
  override def logException(request: Request, requestBody: Option[String], ex: Throwable): Unit = client.logException(request, requestBody, ex)

  override def close(): Unit = client.close()
}