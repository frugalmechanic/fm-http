/*
 * Copyright 2014 Frugal Mechanic (http://frugalmechanic.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fm.http.server

import fm.common.Implicits._
import fm.common.Logging
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}

object RequestRouter {
  def apply(a: RequestRouter, b: RequestRouter): RequestRouter = OrElseRequestRouter(a, b)
  def apply(a: RequestRouter, b: RequestRouter, rest: RequestRouter*): RequestRouter = MultiRequestRouter(Vector(a, b) ++ rest)
  
  def apply(h: PartialFunction[Request, Future[Response]]): RequestRouter = new DefaultRequestRouter { val handler = h }
}

trait RequestRouter {
  /** Given a request lookup a RequestHandler that can satisfy it */
  def lookup(request: Request): Option[RequestHandler]
  
  /** Called before the web server starts up -- Should be idempotent */
  def beforeStartup(): Unit = {}
  
  /** Called after the web server is running but before ping is enabled -- Should be idempotent */
  def afterStartup(): Unit = {}
  
  /** Called when web server shutdown is requested but before actually shutting down -- Should be idempotent */
  def beforeShutdown(): Unit = {}
  
  /** Called once web server shutdown is completed -- Should be idempotent */
  def afterShutdown(): Unit = {}
  
  /**
   * Run any RequestHandlers returned by this RequestRouter through a RequestFilter
   */
  final def withFilter(filter: RequestFilter): RequestRouter = FilteredRequestRouter(this, filter)
  
  /**
   * Run any RequestHandlers returned by this RequestRouter through an optional RequestFilter
   */
  final def withFilter(filter: Option[RequestFilter]): RequestRouter = if (filter.isDefined) withFilter(filter.get) else this
    
  /**
   * Run any RequestHandlers returned by this RequestRouter through a sequence of RequestFilter
   */
  final def withFilters(filters: TraversableOnce[RequestFilter]): RequestRouter = filters.foldRight(this){ (filter, router) => router.withFilter(filter) }
  
  /**
   * If this router doesn't match the request then try that router
   */
  final def orElse(that: RequestRouter): RequestRouter = OrElseRequestRouter(this, that)
  
  /**
   * If this request router does not match a request then send the request to the specified default handler
   */
  final def withDefaultHandler(handler: RequestHandler): RequestRouter = OrElseRequestRouter(this, SingleHandlerRequestRouter(handler))
  
  /**
   * If a RequestHandler throws an Exception then run this handler.  Useful for showing an Error Page when a request fails
   */
  final def withErrorHandler(handler: RequestHandler)(implicit ec: ExecutionContext): RequestRouter = ErrorHandlerRequestRouter(this, handler)
}

/**
 * Proxies to another RequestRouter
 */
abstract class RequestRouterProxy extends RequestRouterBase {
  protected def self: RequestRouter
  
  def lookup(request: Request): Option[RequestHandler] = self.lookup(request)
  
  final override protected def beforeStartupImpl() : Unit = { beforeStartupLocal() ; self.beforeStartup() }
  final override protected def afterStartupImpl()  : Unit = { afterStartupLocal()  ; self.afterStartup() }
  final override protected def beforeShutdownImpl(): Unit = { beforeShutdownLocal(); self.beforeShutdown() }
  final override protected def afterShutdownImpl() : Unit = { afterShutdownLocal() ; self.afterShutdown() }
  
  //
  // Override these (if you need them)
  //
  protected def beforeStartupLocal() : Unit = {}
  protected def afterStartupLocal()  : Unit = {}
  protected def beforeShutdownLocal(): Unit = {} 
  protected def afterShutdownLocal() : Unit = {}
}

/**
 * Adds wrappers around the lifecycle calls to ensure they only get called once
 */
abstract class RequestRouterBase extends RequestRouter {
  private[this] val _beforeStartup : AtomicBoolean = new AtomicBoolean(false)
  private[this] val _afterStartup  : AtomicBoolean = new AtomicBoolean(false)
  private[this] val _beforeShutdown: AtomicBoolean = new AtomicBoolean(false)
  private[this] val _afterShutdown : AtomicBoolean = new AtomicBoolean(false)
  
  final override def beforeStartup() : Unit = if ( _beforeStartup.compareAndSet(false, true)) beforeStartupImpl()
  final override def afterStartup()  : Unit = if (  _afterStartup.compareAndSet(false, true)) afterStartupImpl()
  final override def beforeShutdown(): Unit = if (_beforeShutdown.compareAndSet(false, true)) beforeShutdownImpl()
  final override def afterShutdown() : Unit = if ( _afterShutdown.compareAndSet(false, true)) afterShutdownImpl()
  
  //
  // Override these (if you need them)
  //
  protected def beforeStartupImpl() : Unit = {}
  protected def afterStartupImpl()  : Unit = {}
  protected def beforeShutdownImpl(): Unit = {}
  protected def afterShutdownImpl() : Unit = {}
}

final case object EmptyRequestRouter extends RequestRouter {
  def lookup(request: Request): Option[RequestHandler] = None
}

final case class OrElseRequestRouter(a: RequestRouter, b: RequestRouter) extends RequestRouter {
  def lookup(request: Request): Option[RequestHandler] = a.lookup(request) orElse b.lookup(request)
  override def beforeStartup():  Unit = { a.beforeStartup() ; b.beforeStartup()  }
  override def afterStartup():   Unit = { a.afterStartup()  ; b.afterStartup()   }
  override def beforeShutdown(): Unit = { a.beforeShutdown(); b.beforeShutdown() }
  override def afterShutdown():  Unit = { a.afterShutdown() ; b.afterShutdown()  }
}

final case class ErrorHandlerRequestRouter(router: RequestRouter, errorHandler: RequestHandler)(implicit ec: ExecutionContext) extends RequestRouter with Logging {
  def lookup(request: Request): Option[RequestHandler] = router.lookup(request).map{ wrap }
  
  /**
   * There are 2 places errors can be thrown.  Either by the RequestHandler or the Future
   * that the RequestHandler returns.  We catch both of them.
   */
  private def wrap(handler: RequestHandler): RequestHandler = (request: Request) => {
    try {
      handler(request).recoverWith{ 
        case ex: Throwable =>
          log(ex)
          errorHandler(request)
      }
    } catch {
      case ex: Throwable =>
        log(ex)
        errorHandler(request)
    }
  }
  
  private def log(ex: Throwable): Unit = {
    logger.error(ex)
  }
  
  override def beforeStartup():  Unit = router.beforeStartup()
  override def afterStartup():   Unit = router.afterStartup()
  override def beforeShutdown(): Unit = router.beforeShutdown()
  override def afterShutdown():  Unit = router.afterShutdown()
}

final case class MultiRequestRouter(routers: Seq[RequestRouter]) extends RequestRouter {
  def lookup(request: Request): Option[RequestHandler] = {
    routers.foreach { router: RequestRouter =>
      val res: Option[RequestHandler] = router.lookup(request)
      if (res.isDefined) return res
    }
    
    None
  }
  
  override def beforeStartup():  Unit = routers.foreach{ _.beforeStartup()  }
  override def afterStartup():   Unit = routers.foreach{ _.afterStartup()   }
  override def beforeShutdown(): Unit = routers.foreach{ _.beforeShutdown() }
  override def afterShutdown():  Unit = routers.foreach{ _.afterShutdown()  }
}

/**
 * A RequestRouter that always returns the same handler which should be useful for 404 pages
 */
final case class SingleHandlerRequestRouter(handler: RequestHandler) extends RequestRouter {
  private[this] val res: Option[RequestHandler] = Some(handler)
  def lookup(request: Request): Option[RequestHandler] = res
}

/**
 * A RequestRouter that will dispatch to another RequestRouter based on the virtual host name.
 * 
 * Note: The most specific match wins
 * 
 *  Map(
 *    "frugalmechanic.com" -> ???   // Matches just "frugalmechanic.com"
 *    "*.frugalmechanic.com" -> ??? // Matches any.level.of.sub.domain.frugalmechanic.com (unless there are other more explicit entries)
 *    "*" -> ???                    // Default catch all
 *  )
 */
final case class VirtualHostRequestRouter(map: Map[String, RequestRouter]) extends RequestRouter {
  require(map.keys.forall{ _.isNotBlank }, "Found a blank entry in the virtual host map: "+map)
  
  def lookup(request: Request): Option[RequestHandler] = {
    val host: String = request.host.getOrElse("")
    val res: Option[RequestRouter] = map.get(host) orElse lookupWildCards(stripFirstSubDomain(host))
    res.flatMap{ _.lookup(request) }
  }
  
  private def routers: Iterable[RequestRouter] = map.values
  
  override def beforeStartup():  Unit = routers.foreach{ _.beforeStartup()  }
  override def afterStartup():   Unit = routers.foreach{ _.afterStartup()   }
  override def beforeShutdown(): Unit = routers.foreach{ _.beforeShutdown() }
  override def afterShutdown():  Unit = routers.foreach{ _.afterShutdown()  }
  
  @scala.annotation.tailrec
  private def lookupWildCards(host: String): Option[RequestRouter] = {
    if (host.isBlank) return map.get("*")
    
    val res: Option[RequestRouter] = map.get("*."+host)
    if (res.isDefined) return res
    
    lookupWildCards(stripFirstSubDomain(host))
  }

  private def stripFirstSubDomain(host: String): String = {
    val idx: Int = host.indexOf('.')
    if (idx === -1) "" else host.substring(idx + 1)
  }
}