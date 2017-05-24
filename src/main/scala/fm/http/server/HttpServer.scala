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

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.{Channel, ChannelInitializer, ChannelOption, ChannelPipeline}
import io.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.{HttpRequestDecoder, HttpResponseEncoder}
import io.netty.handler.stream.ChunkedWriteHandler
import io.netty.util.concurrent.GlobalEventExecutor
import fm.common.Implicits._
import fm.common.{Logging, ScheduledTaskRunner}
import fm.http.HttpExecutionContext

object HttpServer {
  
  /**
   * Simple ThreadFactory that allows us to name the threads something reasonable and set the daemon flag
   */
  private final class ThreadFactory(name: String, daemon: Boolean) extends java.util.concurrent.ThreadFactory {
    private[this] val count = new java.util.concurrent.atomic.AtomicInteger(0)
    def newThread(r: Runnable): Thread = {
      val thread: Thread = new Thread(r, name+"-"+count.incrementAndGet())
      thread.setDaemon(daemon)
      thread
    }
  }
  
  private final class ShutdownHookThread(name: String, _server: HttpServer) extends Thread("WebServer Shutdown Hook - "+name) with Logging {
    // Using a weak reference so this thread doesn't prevent the WebServer from being
    // GC'ed (which is useful for SBT unit testing where the JVM stays up a long time)
    private[this] val server = new scala.ref.WeakReference(_server)
    
    override def run: Unit = try {
      server.get.foreach{ server: HttpServer => Await.result(server.shutdown(), Duration.Inf) }
    } catch {
      case ex: Throwable => logger.error(s"Caught Exception in WebServer ($name) Shutdown Hook: "+ ex)
    }
  }
}

final case class HttpServer (port: Int = 8080, router: RequestRouter, authKey: String, serverOptions: HttpServerOptions = HttpServerOptions.default) extends Logging {
  private[this] val name: String = s"WebServer on Port $port"
  private[this] val shutdownHookThread: Thread = new HttpServer.ShutdownHookThread(name, this)
  private[this] val controlHandler = ControlHandler(this, authKey)
  private[this] val completeRouter: RequestRouter = controlHandler orElse router
  
  def enablePing():  Unit = controlHandler.enabled = true
  def disablePing(): Unit = controlHandler.enabled = false
  
  private[this] val bossGroup: NioEventLoopGroup = new NioEventLoopGroup(0, new HttpServer.ThreadFactory("fm-http-server-boss", daemon = false))
  private[this] val workerGroup: NioEventLoopGroup = new NioEventLoopGroup(0, new HttpServer.ThreadFactory("fm-http-server-worker", daemon = false))
  
  /** A flag to indicate that we've already called the shutdown() method to avoid calling it twice */
  private[this] val isShuttingDown: AtomicBoolean = new AtomicBoolean(false)
  
  // This is for executing Scala Future callbacks in the WebRequestHandler, it uses the workerGroup which is the same
  // executor that the Netty ChannelFutures use for their callbacks.
  implicit val executionContext: ExecutionContext = HttpExecutionContext.global
  implicit val timer: ScheduledTaskRunner = HttpExecutionContext.timer
  
  private[this] val allChannels: ChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  
  completeRouter.beforeStartup()
  
  private[this] val serverChannel: Channel = {
    val b: ServerBootstrap = new ServerBootstrap
    b.group(bossGroup, workerGroup)
    b.channel(classOf[NioServerSocketChannel])
    b.option[Integer](ChannelOption.SO_BACKLOG, 1024)
    b.localAddress(port)
    b.childOption[java.lang.Boolean](ChannelOption.TCP_NODELAY, true)
    b.childOption[java.lang.Boolean](ChannelOption.AUTO_READ, false)  // ChannelHandlerContext.read() must be explicitly called when we want a message read
    b.childHandler(new ChannelInitializer[SocketChannel] {
       def initChannel(ch: SocketChannel): Unit = {
         val p: ChannelPipeline = ch.pipeline()
         
         p.addLast("decoder",       new HttpRequestDecoder())
         p.addLast("encoder",       new HttpResponseEncoder())
         p.addLast("compressor",    new NettyContentCompressor())
         p.addLast("chunkedWriter", new ChunkedWriteHandler())
         p.addLast("handler",       new NettyHttpServerPipelineHandler(allChannels, executionContext, completeRouter, serverOptions))
         
       }
    })
    
    b.bind().sync().channel()
  }
  
  registerShutdownHook()
  
  logger.info(s"WebServer Started on Port $port")
  
  completeRouter.afterStartup()
  
  def awaitShutdown(): Unit = Await.result(shutdownFuture, Duration.Inf)
  
  /** The Promise we will complete when the server is fully shutdown */
  //private[this] val shutdownPromise: Promise[Unit] = Promise[Unit]()
  private[this] def shutdownFuture: Future[_] = workerGroup.terminationFuture()
    
  def shutdown(): Future[_] = {
    
    // Make this this method is only called once
    if (!isShuttingDown.compareAndSet(false /* expected value */, true /* value we want to set to */)) return shutdownFuture
        
    logger.info(s"Shutting Down $name")
    
    deregisterShutdownHook()
    
    logger.trace(s"$name - Closing Server Channel")
    
    for {
      _ <- Future{ wrapRouterLifecycleCall("beforeShutdown") { completeRouter.beforeShutdown() } }(scala.concurrent.ExecutionContext.Implicits.global)
      // Close out the serverChannel to stop accepting incoming connections
      _ <- serverChannel.close()
      // Wait for channels to finish processing
      _ <- waitForChannelsToFinish(60)
      // Force close any channels still open
      _ <- allChannels.close()
      // Request shutdown of the boss/worker groups waiting for there
      // to be no new tasks submitted for 1 second (the quiet period)
      // up to a total of 15 seconds (the timeout).
      _ <- bossGroup.shutdownGracefully(1, 15, TimeUnit.SECONDS)
    } {
      // This needs to be in here since it's the one executing our future callbacks
      workerGroup.shutdownGracefully(1, 15, TimeUnit.SECONDS)
      
      // Also run the afterShutdown() hook
      Future{ wrapRouterLifecycleCall("afterShutdown") { completeRouter.afterShutdown() } }(scala.concurrent.ExecutionContext.Implicits.global)
    }
    
    shutdownFuture
  }
  
  private def wrapRouterLifecycleCall(name: String)(f: => Unit): Unit = try {
    f
  } catch {
    case ex: Throwable => logger.error(s"Caught Exception calling $name", ex)
  }
  
  private def waitForChannelsToFinish(seconds: Int): Future[Unit] = {
    logger.trace(s"$name - Waiting for Channels to finish processing")
    val promise = Promise[Unit]()
    waitForChannelsToFinish(seconds, promise)
    promise.future
  }
  
  private def waitForChannelsToFinish(seconds: Int, promise: Promise[Unit]): Unit = {    
    import scala.collection.JavaConverters._
    
    val outstandingRequests: Int = allChannels.asScala.count{ NettyHttpServerPipelineHandler.isProcessingRequest }
    
    if (seconds === 0 || outstandingRequests === 0) {
      if (outstandingRequests > 0) logger.warn(s"Still waiting for $outstandingRequests requests to finish processing")
      promise.success(())
    } else {
      logger.trace(s"Still Waiting for up to $seconds seconds for $outstandingRequests requests to finish processing")
      workerGroup.schedule(new Runnable{ override def run(): Unit = waitForChannelsToFinish(seconds - 1, promise) }, 1, TimeUnit.SECONDS)
    }
  }
  
  private def registerShutdownHook(): Unit = Runtime.getRuntime.addShutdownHook(shutdownHookThread)

  private def deregisterShutdownHook(): Unit = try {
    if (Thread.currentThread.getName != shutdownHookThread.getName) Runtime.getRuntime.removeShutdownHook(shutdownHookThread)
  } catch {
    case _: IllegalStateException =>
  }
  
}