package fm.http.server

import scala.concurrent.Future

trait Auth extends RequestFilter {  
  /**
   * This should be implemented by child classes
   */
  protected def requireAuthImpl(request: Request)(action: => Future[Response]): Future[Response]

  /**
   * Require that the request be authenticated before executing the action
   */
  final def requireAuth(request: Request)(action: => Future[Response]): Future[Response] = requireAuthImpl(request)(action)
  
  /**
   * Require that the request be authenticated before executing the action
   */
  final def requireAuth(action: => Future[Response])(implicit request: Request): Future[Response] = requireAuthImpl(request)(action)

  /**
   * RequestFilter.handle implementation
   */
  final def handle(request: Request, handler: RequestHandler): Future[Response] = {
    requireAuthImpl(request){ handler(request) }
  }
}