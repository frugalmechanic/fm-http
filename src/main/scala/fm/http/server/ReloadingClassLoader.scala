package fm.http.server

import java.io.{File, InputStream}
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader, URLConnection}
import java.security.{ProtectionDomain, Policy, CodeSource, CodeSigner}
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import scala.util.Try

/**
 * 
 * 
 * WARNING: This CANNOT be a case class because we need the equals method to check for reference equality
 *          in case instances of this class are stored in HashMaps (e.g. in scala.reflect.runtime.JavaMirrors).
 *          Otherwise 2 ReloadingClassLoader with the same argument will look equal to each other even though
 *          they have loaded different versions of classes.  This will cause stuff like:
 *          
 *          java.lang.ClassCastException: fm.catalog.website.CatalogSite cannot be cast to fm.catalog.website.CatalogSite
 */
final class ReloadingClassLoader(allowedPackages: Seq[String], parent: ClassLoader, debug: Boolean) extends ClassLoader(parent) {
  
  override def toString(): String = s"ReloadingClassLoader($allowedPackages, $parent, $debug)@"+System.identityHashCode(this)
  
  /**
   * This serves 2 purposes:
   *   1 - Track lastModified timestamps for classes we have loaded
   *   2 - Track classes that are valid with a lastModified of Long.MinValue
   */
  private[this] val timestamps = new ConcurrentHashMap[String, Long]()

  def seenClasses: Vector[String] = timestamps.keys().asScala.toVector

  /** Add classes we expect to exist (i.e. we will wait for them to become available) */
  def addValidClasses(names: Iterable[String]): Unit = names.foreach { name: String =>
    timestamps.putIfAbsent(name, Long.MinValue)
  }
  
  private[this] val protectionDomain: ProtectionDomain = {
    val tmp = new ProtectionDomain(null, Policy.getPolicy().getPermissions(new CodeSource(null, null.asInstanceOf[Array[CodeSigner]])))    
    // Set the default protected domain such that it doesn't reference this class 
    // loader which would prevent unloading.
    val f = classOf[ClassLoader].getDeclaredField("defaultDomain")
    f.setAccessible(true)
    f.set(this, tmp)
    tmp
  }

  private case class ResourceInfo(name: String, lastModified: Long, bytes: Array[Byte])
  
  private def tryGetResourceInfo(name: String, tries: Int): ResourceInfo = tryGet(name, tries)(getResourceInfo)
    
  @scala.annotation.tailrec
  private def tryGet[A](name: String, tries: Int)(f: String => Option[A]): A = {
    f(name) match {
      case Some(res) => res
      case None =>
        if (tries <= 0 || !timestamps.containsKey(name)) throw new ClassNotFoundException(name)
        println(s"Waiting for $name to become available...")
        Thread.sleep(1000)
        tryGet(name, tries - 1)(f)
    }
  }
  
  private def lookupLastModified(name: String): Long = try {
    val url: URL = getResource(resourceNameForClass(name))
    val conn: URLConnection = url.openConnection()
    // Don't use any cached data since we want to see changes
    conn.setUseCaches(false)
    conn.getLastModified()
  } catch {
    case ex: Exception => -1
  }
  
  private def getResourceInfo(name: String): Option[ResourceInfo] = Try {
    val url: URL = getResource(resourceNameForClass(name))
    val conn: URLConnection = url.openConnection()
    // Don't use any cached data since we want to see changes
    conn.setUseCaches(false)
    
    val lastModified: Long = conn.getLastModified()
    val length: Int = conn.getContentLength()
    val bytes: Array[Byte] = new Array(length)
    val is: InputStream = conn.getInputStream()
    is.read(bytes, 0, length)
    is.close()
    
    ResourceInfo(name, lastModified, bytes)
  }.toOption

  private def isAllowedClass(name: String): Boolean = {
    allowedPackages.exists{ name.startsWith }
  }

  // findLoadedClass is a protected method so in order to get access to it we
  // have to play the reflection game to unprotect the method
  private[this] val parentFindLoadedClassMethod: Method = {
    val m = classOf[ClassLoader].getDeclaredMethod("findLoadedClass", classOf[String])
    m.setAccessible(true)
    m
  }

  private def parentFindLoadedClass(name: String): Class[_] = parentFindLoadedClassMethod.invoke(parent, name).asInstanceOf[Class[_]]

  private def debug(msg: => String): Unit = if (debug) println(""+System.identityHashCode(this)+": "+msg)
  
  override def loadClass(name: String, resolve: Boolean): Class[_] = synchronized {
    //debug(s"loadClass($name, $resolve)")

    // If not allowed then just load from the parent
    if (!isAllowedClass(name)) {
      //debug(s"parentLoadClass($name)")
      return try{
        parentLoadClass(name, resolve)
        //super.loadClass(name, resolve)
      } catch {
        case ex: ClassNotFoundException =>
          //debug(s"parentLoadClass($name, $resolve) <<< ClassNotFoundException >>>")
          throw ex
      }
    }
    
    // Check to see if we already loaded the class
    var clazz: Class[_] = findLoadedClass(name)
    debug(s"findLoadedClass($name) => "+(null != clazz))

    if (null == clazz) {
      // Lookup the class locally
      val info: ResourceInfo = tryGetResourceInfo(name, 120)
      clazz = defineClass(name, info.bytes, 0, info.bytes.length, protectionDomain)
      debug(s"defineClass($name)")
      timestamps.put(name, info.lastModified)
    }

    if(resolve) resolveClass(clazz)

    clazz
  }

  private def parentLoadClass(name: String, resolve: Boolean): Class[_] = {
    val clazz: Class[_] = parent.loadClass(name)
    if(resolve) resolveClass(clazz)
    clazz
  }

  /**
   * Have any of the classes that we've loaded been modified?
   */
  def isModified: Boolean = modifiedClasses.nonEmpty
  
  /**
   * Any classes that we've loaded that have been modified
   * 
   * Note: Needs to be synchronized so that it's not run
   *       while we are in the middle of loading a class.
   */
  def modifiedClasses: Set[String] = synchronized {
    timestamps.asScala.filterNot{ case (name, timestamp) =>
      // Ignore classes that are valid but we haven't loaded
      timestamp == Long.MinValue
    }.filter { case (name, timestamp) => 
      lookupLastModified(name) != timestamp
    }.map{ case (name, _) => name }.toSet
  }
  
  private def resourceNameForClass(name: String) = {
    val tmp = name.replace(".", "/")
    if (tmp.endsWith(".class")) tmp else tmp+".class"
  }
}