package fm.http.server

import fm.common.{ClassUtil, Logging}
import java.lang.ref.WeakReference
import java.io.InputStream
import java.net.URL
import java.util.Enumeration

object ReloadingClassLoaderHolder {
  def defaultParentClassLoader: ClassLoader = getClass.getClassLoader()
}

/**
 * Holds a reference to a ReloadingClassLoader
 * 
 * I forget why this originally extended URLClassLoader.  I think it might be because some
 * code out there expects the ClassLoader to be a URLClassLoader.
 * 
 * WARNING: See the warning in ReloadingClassLoader about why this is NOT a case class
 */
final class ReloadingClassLoaderHolder(
  allowedPackages: Seq[String],
  parent: ClassLoader = ReloadingClassLoaderHolder.defaultParentClassLoader,
  debug: Boolean = false
) extends ClassLoader(parent) with Logging {
  
  // This is our current ReloadingClassLoader
  @volatile private[this] var classLoader: ReloadingClassLoader = {
    // Lookup all classes we are interested in and add them as "validClasses" to
    // the ReloadingClassLoader such that if we call loadClass on one of the "validClasses"
    // and the class doesn't exist on disk (perhaps because it's being compiled) we will
    // know to wait for the class to show up instead of throwing a ClassNotFoundException
    val classNames: Seq[String] = allowedPackages.flatMap{ ClassUtil.findClassNames(_, parent) }
    
    val cl = newReloadingClassLoader()
    cl.addValidClasses(classNames)
    cl
  }
  
  // This tracks all class loaders we've created (that haven't been GC'd)
  @volatile private[this] var oldClassLoaders: Vector[WeakReference[ReloadingClassLoader]] = Vector.empty
  
  def reload(): Unit = synchronized {
    logger.warn("Reloading Classes...")
    val oldLoader: ReloadingClassLoader = classLoader
    classLoader = newReloadingClassLoader()
    classLoader.addValidClasses(oldLoader.seenClasses)
    trackAndReportOldClassloaders(oldLoader)
    
    // Request a GC to try and clean up old class loaders
    System.gc()
  }
  
  private def newReloadingClassLoader(): ReloadingClassLoader = new ReloadingClassLoader(allowedPackages, parent, debug)
  
  private def trackAndReportOldClassloaders(cl: ReloadingClassLoader): Unit = {
    oldClassLoaders = oldClassLoaders.filter{ _.get != null }
    if (oldClassLoaders.nonEmpty) logger.warn("Old Class Loaders not yet garbage collected: "+oldClassLoaders.size)
    oldClassLoaders = oldClassLoaders :+ new WeakReference(cl)
  }

  def isModified: Boolean = classLoader.isModified
  
  override def clearAssertionStatus(): Unit = classLoader.clearAssertionStatus()
  override def getResource(name: String): URL = classLoader.getResource(name)
  override def getResourceAsStream(name: String): InputStream = classLoader.getResourceAsStream(name)
  override def getResources(name: String): Enumeration[URL] = classLoader.getResources(name)
  override def loadClass(name: String): Class[_] = classLoader.loadClass(name)
  override def loadClass(name: String, resolve: Boolean): Class[_] = classLoader.loadClass(name,resolve)
  override def setClassAssertionStatus(className: String, enabled: Boolean): Unit = classLoader.setClassAssertionStatus(className, enabled)
  override def setDefaultAssertionStatus(enabled: Boolean): Unit = classLoader.setDefaultAssertionStatus(enabled)
  override def setPackageAssertionStatus(packageName: String, enabled: Boolean): Unit = classLoader.setPackageAssertionStatus(packageName, enabled)
}
