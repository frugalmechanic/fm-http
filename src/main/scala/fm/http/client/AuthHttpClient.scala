package fm.http.client

import fm.http.MutableHeaders
import scala.concurrent.Future
import scala.concurrent.duration.Duration

abstract class AuthHttpClient extends HttpClientProxy {
  
  /**
   * The Authorization header line for a request (if any)
   * 
   * This is added to the request before making the HTTP call
   */
  protected def makeAuthorization(request: Request): Option[String]
  
  /**
   * The Authorization header line for a request and 401 response
   * 
   * If a Some is returned then we will re-make the HTTP request with the given
   * Authorization header
   */
  protected def makeAuthorization(request: Request, response: Response): Option[String]
  
  final override def execute(request: Request, timeout: Duration): Future[AsyncResponse] = {
    val requestWithAuth: Request = withAuthorization(request, makeAuthorization(request))
    
    super.execute(requestWithAuth, timeout) flatMap { response: AsyncResponse =>
      if (response.status.isUnauthorized) {
        makeAuthorization(request, response) match {
          case None    => Future.successful(response) // No auth available so return original response
          case Some(a) =>
            response.close() // Close out the original response
            super.execute(withAuthorization(request, a), timeout) // Retry with auth
        }
      } else {
        // Return original response
        Future.successful(response)
      }
    }
  }
  
  private def withAuthorization(request: Request, auth: Option[String]): Request = {    
    auth match {
      case None => request
      case Some(a) => withAuthorization(request, a)
    }
  }
  
  private def withAuthorization(request: Request, auth: String): Request = {
    val headers: MutableHeaders = request.headers.toMutableHeaders
    headers.authorization = auth
    request.withHeaders(headers.toImmutableHeaders)
  }
  
//  private def withAuthorization(request: Request, auth: Option[String]): Option[Request] = {
//    auth map { a: String => 
//      val headers: MutableHeaders = request.headers.toMutableHeaders
//      headers.authorization = a
//      request.withHeaders(headers.toImmutableHeaders)
//    }
//  }
}