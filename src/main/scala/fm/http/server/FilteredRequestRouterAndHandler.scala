package fm.http.server

import scala.concurrent.{ExecutionContext, Future}

final class FilteredRequestRouterAndHandler(routerAndHandler: RequestRouterAndHandler, filter: RequestFilter) extends RequestRouterAndHandler {

  final override def lookup(request: Request): Option[RequestHandler] = {
    routerAndHandler.lookup(request).map{ _.withFilter(filter) }
  }

  final override def apply(request: Request)(implicit executor: ExecutionContext): Future[Response] = {
    filter.handle(request, routerAndHandler)
  }

  override def beforeStartup(): Unit = routerAndHandler.beforeStartup()
  override def afterStartup(): Unit = routerAndHandler.afterStartup()
  override def beforeShutdown(): Unit = routerAndHandler.beforeShutdown()
  override def afterShutdown(): Unit = routerAndHandler.afterShutdown()
}
