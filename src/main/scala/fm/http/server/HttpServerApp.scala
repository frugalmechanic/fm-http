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

import com.frugalmechanic.optparse._
import fm.common.Implicits._
import fm.common.Logging
import java.io.{BufferedReader, InputStreamReader, OutputStream, PrintStream}
import java.net.{URL, HttpURLConnection}
import java.lang.management.ManagementFactory
import jnr.posix.POSIXFactory
import jnr.posix.util.DefaultPOSIXHandler
import org.slf4j.LoggerFactory
import scala.util.control.Breaks._
import scala.collection.JavaConverters._

abstract class HttpServerApp extends Logging {
  /** The RequestRouter that will handle requests */
  protected def router: RequestRouter
  
  /** The secret key used by the ControlHandler */
  protected def AuthKey: String
  
  /** The email username to use (if using email logging)  */
  protected def EmailUser: String
  
  /** The email password to use (if using email logging)  */
  protected def EmailPass: String
  
  private[this] val DETATCH_STRING: String = "\u0000"*4 // 4 NULL Characters
  private[this] val IS_CHILD_PROPERTY_KEY: String = "fm.webapp.is_child_process"
  private[this] val isChildProcess: Boolean = "true" === System.getProperty(IS_CHILD_PROPERTY_KEY)
  
  private[this] val verbosePosix: Boolean = true

  private[this] val POSIX = POSIXFactory.getPOSIX(FMPOSIXHandler(verbosePosix), true)
  require(POSIX.isNative, "Expected Native POSIX implementation!")
  
  object Options extends OptParse {
    val detatch = BoolOpt(desc="Daemonize after booting up")
    val start   = BoolOpt(desc="Alias for --detatch", enables=List(detatch))
    val stop    = BoolOpt(desc="Stop any running web servers (on the ports we care about)")
    val status  = BoolOpt(desc="Display the status of any running web servers")
    val port    = MultiStrOpt(desc="Which port to run on")
    val email   = BoolOpt(desc="Enable Email Logging")
  }
  
  def main(args: Array[String]): Unit = {
    // Exit on uncaught exceptions
    Thread.currentThread.setUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler{
      def uncaughtException(t:Thread, ex:Throwable) {
        logger.error("Uncaught Exception!", ex)
        System.exit(1)
      }
    })
    
    Options.parse(args)
    
    val ports: Set[Int] = parsePorts(Options.port.getOrElse(Seq("8080")))

    require(ports.nonEmpty, "Missing --port option")
    require(ports.size === 1 || ports.size === 2, "We only support 1 or 2 ports")
    
    println("Using port(s): "+ports.mkString(", "))
    
    val emailLogging: Boolean = Options.email

    if      (Options.start)  doStart(ports, emailLogging = emailLogging)  // Startup in background
    else if (Options.stop)   doStop(ports)    // Stop running servers
    else if (Options.status) doStatus(ports)  // Show status
    else                     doRun(ports, detatch = false, emailLogging = emailLogging) // Run in the foreground
  }

  /**
   * Parses the value of the --port option
   */
  private def parsePorts(ports: Seq[String]): Set[Int] = try {
    ports.flatMap{ _.split("""[\s,]+""") }.map{ _.toInt }.toSet
  } catch { case ex: NumberFormatException => println("Invalid Port: "+ex.getMessage); sys.exit(-1) }
  
  /**
   * Startup the web server in the background (or if we are in the child process run it in the foreground)
   */
  private def doStart(ports: Set[Int], emailLogging: Boolean): Unit = {
    if (isChildProcess) doRun(ports, detatch = true, emailLogging = emailLogging) else doExec(ports)
  }
  
  private def doStop(ports: Set[Int]): Unit = shutdownPorts(ports.filter{ alive })
  
  private def doStatus(ports: Set[Int]): Unit = ports.foreach{ alive }

  /**
   * Run the Web Server
   */
  private def doRun(ports: Set[Int], detatch: Boolean, emailLogging: Boolean): Unit = {    
    // Figure out which port we should listen on    
    val (usedPorts, availPorts) = ports.partition{ alive }
    
    if (availPorts.isEmpty) { logger.error("All Ports in use"); sys.exit(-1) }
    
    val port: Int = availPorts.head
    logger.info("Using Port: "+port)
    
    // Startup the Server
    val server: HttpServer = HttpServer(port, router, AuthKey)
    
    // Check that the server is alive and responding
    assert(alive(port), "Server not alive?!")
    
    server.enablePing()
    
    if (usedPorts.nonEmpty) {
      logger.info("Allowing Load Balancer to pickup new server...")
      Thread.sleep(5000)
      
      logger.info("Shutting down old servers...")
      shutdownPorts(usedPorts)
    }
    
    if (emailLogging) setupEmailLogging()

    logger.info("Successfully Started ("+POSIX.getpid+")")

    if (detatch) {
      logger.info("Redirecting STDOUT/STDERR to log and Detatching...")
      println(DETATCH_STRING)
      
      POSIX.setsid()

      System.out.close()
      System.err.close()

      removeConsoleLogging()
      
      System.setOut(new PrintStream(new LoggingOutputStream("stdout"), true, "UTF-8"))
      System.setErr(new PrintStream(new LoggingOutputStream("stderr"), true, "UTF-8"))
    }

    // Block until shutdown is requested
    server.awaitShutdown()
    
    // Shutdown Logging
    import ch.qos.logback.classic.LoggerContext
    LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext].stop()
  }
  
  private def doExec(ports: Set[Int]): Unit = {
    println("[Launcher] Launching WebApp")
    println("[Launcher] "+makeCommand(ports, includeClasspath = false).mkString(" "))

    val process: Process = new ProcessBuilder(makeCommand(ports).asJava).redirectErrorStream(true).start

    val reader: BufferedReader = new BufferedReader(new InputStreamReader(process.getInputStream))
    
    // Read and print from the child until we see the DETATCH_STRING
    var line: String = reader.readLine
    while(null != line && line != DETATCH_STRING) {
      println("[Child] "+line)
      line = reader.readLine
    }

    process.getInputStream.close()
    process.getOutputStream.close()
    process.getErrorStream.close()

    println("[Launcher] Done")
  }
  
  /**
   * Create the exec command that will be used to launch the child process
   */
  private def makeCommand(ports: Set[Int], includeClasspath: Boolean = true): Seq[String] = {
    val JAVA_HOME: String = System.getProperty("java.home")
    
    val JAVA_OPTS: Seq[String] = ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toIndexedSeq ++ Seq(
      "-D"+IS_CHILD_PROPERTY_KEY+"=true"
    )
    
    val clazz: String = getClass.getName
    val APP: String = if (clazz.endsWith("$")) clazz.substring(0, clazz.length-1) else clazz
    val CLASSPATH: String = System.getProperty("java.class.path")
    
    val args = Vector.newBuilder[String]
    // Java Command
    args += JAVA_HOME+"/bin/java"
    
    // Java Options
    args ++= JAVA_OPTS
    if (includeClasspath) args ++= Seq("-classpath", CLASSPATH)
    
    // The class we are running
    args += APP
    
    // APP Command Line Options:
    args += "--start"
    if (ports.nonEmpty) args ++= Seq("--port", ports.mkString(","))
    
    args.result
  }
  
  def shutdownPorts(ports: Set[Int]): Unit = {
    ports.foreach{ shutdown }

    var waitingForPorts: Set[Int] = ports.toSet
    logger.info("Waiting for "+waitingForPorts+" to shutdown...")

    breakable{
      // Wait up to 90 seconds for the servers to stop responding
      for(i <- 1 to 90) {
        waitingForPorts = waitingForPorts.filter{alive}
        if (waitingForPorts.isEmpty) break
        Thread.sleep(1000)
      }
    }

    if (waitingForPorts.isEmpty) {
      logger.info("All used ports shut down.")
    } else {
      logger.error("WARNING: The following servers did not shut down: "+waitingForPorts)
    }
  }

  def removeConsoleLogging(): Unit = {
    import ch.qos.logback.classic.LoggerContext

    val ctx = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = ctx.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)

    // Detach anything that would log to STDOUT/STDERR
    rootLogger.detachAppender("STDOUT")
    rootLogger.detachAppender("STDERR")
    rootLogger.detachAppender("CONSOLE")
    
    // Detatch the event logger
    val eventLogger = ctx.getLogger("FMWebEventLogger")
    eventLogger.detachAppender("EVENTS_STDOUT")
  }

  // TODO: clean up email logging to use something like: http://logback.qos.ch/recipes/emailPerTransaction.html
  // More Info here too:  http://logback.qos.ch/manual/appenders.html#OnMarkerEvaluator
  def setupEmailLogging(): Unit = {
    logger.info("Setting up email logging...")

    import ch.qos.logback.classic.LoggerContext
    import ch.qos.logback.classic.net.SMTPAppender

    val ctx: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

    val rootLogger = ctx.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
    val emailAppender: SMTPAppender = ctx.getLogger("__EMAIL__").getAppender("EMAIL").asInstanceOf[SMTPAppender]

    assert(null != emailAppender, "Email Appender is null!")
    if (null != emailAppender) {
      if (EmailUser.isNotBlank) emailAppender.setUsername(EmailUser)
      if (EmailPass.isNotBlank) emailAppender.setPassword(EmailPass)
      emailAppender.start() // start() has to be called for it to pick up the new username/password
      rootLogger.addAppender(emailAppender)
    }
  }
  
  def alive(port: Int): Boolean = get("/_alive", port) === 200
  def ping(port: Int): Boolean = get("/_ping", port) === 200
  def shutdown(port: Int): Boolean = get("/_shutdown", port) === 200
  
  def get(path: String, port: Int): Int = {
    val url: String = "http://localhost:"+port+path
    val code: Int = try {
      val conn: HttpURLConnection = new URL(url+"?key="+AuthKey).openConnection.asInstanceOf[HttpURLConnection]
      val res: Int = conn.getResponseCode()
      conn.getInputStream().close()
      conn.disconnect()
      res
    } catch { case ex: java.net.ConnectException => -1 }

    logger.info("GET "+url+" => "+code)

    code
  }
  
  private final case class FMPOSIXHandler(verbose: Boolean) extends DefaultPOSIXHandler {
    override def isVerbose(): Boolean = verbose
  }
  
  private final case class LoggingOutputStream(name: String) extends OutputStream {
    private[this] val logger = LoggerFactory.getLogger(name)

    override def write(bytes: Array[Byte]): Unit = write(bytes, 0, bytes.length)
    def write(b: Int): Unit = write(Array(b.toByte))

    override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
      if (logger.isInfoEnabled) logger.info(new String(bytes, off, len, "UTF-8"))
    }
    
  }
}
