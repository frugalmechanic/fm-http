package fm.http.server

import fm.common.URL
import fm.http.{Headers, Status}
import scala.concurrent.Future

/**
 * If the X-SSL header is not set then this redirects to the HTTPS version
 * of the requested URL.
 * 
 * TODO: probably need to do something smarter with POST/PUT/etc requests.
 */
object RequireSSLFilter extends RequestFilter {
  def handle(request: Request, handler: RequestHandler): Future[Response] = {
    // Make sure the X-SSL header is set
    if (request.headers.xSSL.getOrElse(false)) return handler(request)
    
    // Otherwise redirect to the SSL version
    val location: URL = URL(s"https://${request.hostWithPort.getOrElse("")}${request.uri}")
    val headers: Headers = Headers.noCache("Location" -> location.toString)
    Future.successful(Response.plain(Status.FOUND, headers))
  }
}