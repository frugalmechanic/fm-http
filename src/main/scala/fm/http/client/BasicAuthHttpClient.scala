package fm.http.client

import fm.common.Base64
import fm.http.Headers
import java.nio.charset.StandardCharsets

final class BasicAuthHttpClient(user: String, pass: String, val client: HttpClient) extends AuthHttpClient {
  private[this] val encoded: String = Headers.makeBasicAuthorization(user, pass)
  
  protected def makeAuthorization(request: Request): Option[String] = Some(encoded)
  protected def makeAuthorization(request: Request, response: Response): Option[String] = makeAuthorization(request)
}