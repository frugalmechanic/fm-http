package fm.http.client

import fm.common.Implicits._
import fm.http.MutableHeaders
import io.netty.buffer.ByteBuf
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
    var byteBufToRelease: Option[ByteBuf] = None

    // Can we retry this request if we get a 401 Unauthorized?
    val canRetryRequest: Boolean = request match {
      case full : FullRequest  => full.buf.retain(); byteBufToRelease = Option(full.buf); true // We need to retain() the ByteBuf to prevent a double free
      case async: AsyncRequest => false // Cannot retry due to not having access to the request body
      case file : FileRequest  => true
    }

    val originalAuthorization: Option[String] = makeAuthorization(request)
    val requestWithAuth: Request = withAuthorization(request, originalAuthorization)
    
    val res: Future[AsyncResponse] = super.execute(requestWithAuth, timeout) flatMap { response: AsyncResponse =>
      if (response.status.isUnauthorized) {
        val updatedAuthorization: Option[String] = makeAuthorization(request, response)

        if (!canRetryRequest || updatedAuthorization.isEmpty || updatedAuthorization === originalAuthorization) {
          // There is no authorization value or it matches the original value in which case
          // we would not expect a different response from the server.  So we just return
          // the original response.
          Future.successful(response)
        } else {
          // Close out the original response
          response.close()

          // Retry with the updated authorization -- Note: This code path is for DIGEST auth and not needed for BASIC
          super.execute(withAuthorization(request, updatedAuthorization), timeout)
        }
      } else {
        // Return original response
        Future.successful(response)
      }
    }

    // If this was a FullRequest we need to make sure we release the request ByteBuf when we are complete
    if (byteBufToRelease.isDefined) res.onComplete{ _ => byteBufToRelease.get.release() }

    res
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