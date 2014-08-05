package fm.http.server

import fm.common.Logging

/**
 * A RequestRouter that wraps another RequestRouter which will reload
 * classes that change on the fly.
 * 
 * For best results limit the scope of the reloadablePackages.
 */
final case class ReloadingRequestRouter(
  /** The fully qualified className of the RequestRouter that this ReloadingRequestRouter wraps */
  className: String,
  /** Java/Scala Package Prefixes that are allowed to be reloaded */
  reloadablePackages: Seq[String],
  /** The parent ClassLoader to use */
  parent: ClassLoader = classOf[ReloadingRequestRouter].getClassLoader,
  /** Show debugging output */
  debug: Boolean = false
) extends RequestRouter with Logging {
  
  private[this] val reloadingClassLoader: ReloadingClassLoaderHolder = new ReloadingClassLoaderHolder(reloadablePackages, parent, debug)
  @volatile private[this] var _router: RequestRouter = newRequestRouter()
  
  /** Create a new RequestRouter */
  private def newRequestRouter(): RequestRouter = reloadingClassLoader.loadClass(className).newInstance().asInstanceOf[RequestRouter]
  
  /** Get the current RequestRouter */
  private def router: RequestRouter = synchronized {
    val modifiedClasses: Set[String] = reloadingClassLoader.modifiedClasses
    
    if (modifiedClasses.nonEmpty) {
      //logger.warn("Modified Classes: \n  "+modifiedClasses.mkString("\n  "))
      
      _router.beforeShutdown()
      _router.afterShutdown()
      
      reloadingClassLoader.reload()
      _router = newRequestRouter()
      
      _router.beforeStartup()
      _router.afterStartup()
    }
    
    _router
  }
  
  def lookup(request: Request): Option[RequestHandler] = {
    router.lookup(request)
  }
  
  override def beforeStartup() : Unit = _router.beforeStartup()
  override def afterStartup()  : Unit = _router.afterStartup()
  override def beforeShutdown(): Unit = _router.beforeShutdown()
  override def afterShutdown() : Unit = _router.afterShutdown()
}