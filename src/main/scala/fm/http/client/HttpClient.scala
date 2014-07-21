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
package fm.http.client


import fm.common.Implicits._
import fm.common.{IP, Logging, ScheduledTaskRunner, URL}
import fm.http._
import fm.netty._

import io.netty.bootstrap.Bootstrap
import io.netty.channel.{Channel, ChannelFuture, ChannelInitializer, ChannelOption, ChannelPipeline}
import io.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.{HttpClientCodec, HttpResponseStatus}
import io.netty.handler.codec.socks._
import io.netty.handler.stream.ChunkedWriteHandler
import io.netty.handler.ssl.SslHandler
import io.netty.util.concurrent.GlobalEventExecutor

import java.util.concurrent.{ConcurrentHashMap, TimeoutException, TimeUnit}
import java.io.{Closeable, File, FileNotFoundException, IOException}
import java.lang.ref.WeakReference
import java.net.MalformedURLException

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


/**
 * This holds a single copy of the NioEventLoopGroup / NettyExecutionContext 
 */
object HttpClient extends Logging {

//  def close(): Unit = {
//    workerGroup.shutdownGracefully(1, 15, TimeUnit.SECONDS)
//  }
  
  private val workerGroup: NioEventLoopGroup = new NioEventLoopGroup(0, new ThreadFactory("http-client-worker", daemon=true))
  
  /**
   * This can be imported if you know what you are doing
   */
  implicit val executionContext: ExecutionContext = HttpExecutionContext.global
  
  /**
   * This can be used to schedule tasks
   */
  implicit val timer: ScheduledTaskRunner = HttpExecutionContext.timer
  
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
  
  private val DefaultHeaders: ImmutableHeaders = {
    val h = MutableHeaders()
    h.accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
    h.acceptEncoding = "gzip,deflate"
    h.acceptLanguage = "en-US,en;q=0.8"
    h.userAgent = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)"
    h.toImmutableHeaders
  }
  
  private case class EndPoint(host: String, port: Int, ssl: Boolean, socksProxy: Option[(String, Int)]) {
    def prettyString: String = {
      val scheme = if (ssl) "https" else "http"
      val proxy = socksProxy.map{ case (host,port) => s" via $host:$port" }.getOrElse("")
      s"${scheme}://$host:$port$proxy"
    }
  }
  
  private val IdleCheckDuration: FiniteDuration = 5.seconds
  
  private def enableTimeoutTask[T](promise: Promise[T], duration: Duration): TimeoutTask[T] = {
    val task = new TimeoutTask(new WeakReference(promise))
    val scheduledFuture = timer.schedule(task, duration.asInstanceOf[FiniteDuration])
    
    // Cancel the scheduled task if the future completes normally
    promise.future.onComplete{
      case Success(_) => scheduledFuture.cancel()
      case Failure(ex) => ex match {
        case _: TimeoutTaskTimeoutException => // Ok
        case _ => scheduledFuture.cancel()
      }
    }
    
    task
  }
  
  private def enableIdleTask(client: HttpClient): Unit = {
    timer.schedule(new ConnectionIdleTask(new WeakReference(client)), IdleCheckDuration)
  }
  
  private class ConnectionIdleTask(ref: WeakReference[HttpClient]) extends Runnable {    
    def run(): Unit = {
      val client: HttpClient = ref.get()
      if (null != client) {
        client.closeIdleConnections()
        timer.schedule(this, IdleCheckDuration)
      }
    }
  }
  
  final class TimeoutTaskTimeoutException extends TimeoutException("Request Timed Out")
  
  private class TimeoutTask[T](ref: WeakReference[Promise[T]]) extends Runnable {
    private[this] var _hasRun: Boolean = false
    private[this] var channelRef: WeakReference[Channel] = null
    
    def hasRun: Boolean = _hasRun
    
    def setChannel(ch: Channel): Unit = channelRef = new WeakReference(ch)
    
    def run(): Unit = synchronized {
      require(!_hasRun)
      _hasRun = true
      
      val promise: Promise[T] = ref.get()
      if (null != promise) {
        // Attempt to fail the promise
        if (promise.tryFailure(new TimeoutTaskTimeoutException())) {
          // If we were able to fail the promise then also try to close the channel
          if (null != channelRef) {
            val ch: Channel = channelRef.get
            if (null != ch) ch.close()
          }
        }
      }
      
    }
  }
}

final case class HttpClient(
  socksProxy: Option[(String, Int)] = None,
  defaultMaxLength: Long = 10485760, /* 10MB (which may or may not be 10MB worth of Chars) */
  defaultHeaders: Headers = HttpClient.DefaultHeaders,
  useConnectionPool: Boolean = true, // Should we re-use connections? (Use HTTP Keep Alive?)
  maxConnectionsPerHost: Int = 8,     // Only applies if useConnectionPool is true
  maxConnectionIdleDuration: FiniteDuration = 30.seconds,
  defaultResponseTimeout: Duration = 5.minutes, // The maximum time to wait for a Response
  defaultConnectTimeout: Duration = 30.seconds // The maximum time to wait to connect to a server
) extends Closeable with Logging {
  import HttpClient.{EndPoint, ThreadFactory, TimeoutTask, workerGroup}
  
  require(maxConnectionsPerHost > 0, "maxConnectionsPerHost must be > 0")
  
  implicit val executionContext: ExecutionContext = HttpClient.executionContext
  implicit val timer: ScheduledTaskRunner = HttpClient.timer
  
  /**
   * Perform a HEAD request.  Always returns a FullResponse because the body will be empty
   */
  def head(url: String, headers: Headers = defaultHeaders, timeout: Duration = defaultResponseTimeout): Future[FullResponse] = execute(Request.Head(url, headers), timeout).flatMap{ _.toFullResponse(0) }
  
  /**
   * Perform a GET request returning the FullResponse with a max length for the body
   */
  def getFull(url: String, headers: Headers = defaultHeaders, maxLength: Long = defaultMaxLength, timeout: Duration = defaultResponseTimeout): Future[FullResponse] = getAsync(url, headers, timeout).flatMap{ _.toFullResponse(maxLength) }
  
  /**
   * Perform a GET request returning an AsyncResponse for reading arbitrarily long response bodies
   */
  def getAsync(url: String, headers: Headers = defaultHeaders, timeout: Duration = defaultResponseTimeout): Future[AsyncResponse] = execute(Request.Get(url, headers), timeout)
  
  /**
   * Execute a Request returning the AsyncResponse
   */
  def execute(r: Request, timeout: Duration): Future[AsyncResponse] = {
    URL.tryParse(r.url) match {
      case Failure(ex) => Future.failed(ex)
      case Success(url) => execute0(url, r, timeout)
    }   
  }
  
  def close(): Unit = {
    //endpointMap.values().asScala.foreach{ _.close() }
    //endpointMap.clear()
    allChannels.close().sync()
  }
  
  // Access to this should be synchronized (which means we shouldn't even bother with a ConcurrentHashMap...)
  private[this] val endpointMap: ConcurrentHashMap[EndPoint, WeakReference[ChannelPool]] = new ConcurrentHashMap()
  
  private def closeIdleConnections(): Unit = endpointMap.synchronized {
    val it = endpointMap.values().iterator()
    
    while(it.hasNext()) {
      val pool: ChannelPool = it.next().get
      if (null == pool) it.remove() else pool.closeIdleChannels()
    }
  }
  
  /**
   * Get a channel either from a pool (if useConnectionPool is true) or created directly
   */
  private def getChannel(host: String, port: Int, ssl: Boolean, socksProxy: Option[(String, Int)]): Future[Channel] = {    
    val endPoint: EndPoint = EndPoint(host, port, ssl, socksProxy)
    
    if (useConnectionPool) endpointMap.synchronized {
      val poolRef: WeakReference[ChannelPool] = endpointMap.get(endPoint)
      
      var pool: ChannelPool = if (null != poolRef) poolRef.get else null
      
      if (null == pool) {
        pool = new ChannelPool(endPoint.prettyString, makeNewChannel(endPoint), limit = maxConnectionsPerHost, maxIdleMillis = maxConnectionIdleDuration.toMillis)
        endpointMap.put(endPoint, new WeakReference(pool))
      }
      
      pool.checkout()
    } else {
      makeNewChannel(endPoint)(null)
    }
  }
  
  
  private[this] val traceSep: String = "======================================================================================================================="
  
  private def execute0(url: URL, request: Request, timeout: Duration): Future[AsyncResponse] = {
    if (logger.isTraceEnabled) logger.trace(s"Sending Request:\n$traceSep\n$request\n$traceSep\n")
    else logger.info(s"${request.method.name} $url")
    
    val scheme: String = url.scheme.getOrElse{ return Future.failed(new MalformedURLException("Missing Scheme")) }
    val host: String = url.host.getOrElse{ return Future.failed(new MalformedURLException("Missing Host")) }
    
    if (scheme != "http") return Future.failed(new MalformedURLException("We only support 'http' as a scheme"))

    val port: Int = url.port.getOrElse{ scheme match {
      case "http" => 80
      case "https" => 443
    }}
    
    val ssl: Boolean = scheme == "https"
    
    // Add a "Host:" header if its missing
    val fixedRequest: Request = if (request.headers.host.isEmpty) {
      val newHeaders = request.headers.toMutableHeaders
      newHeaders.host = host
      request.withHeaders(newHeaders)
    } else request
    
    // This if the URI part of the request:  /foo/bar?param=value
    val uri: String = url.path.getOrElse("/")+url.query.map{ "?"+_ }.getOrElse("")
    
    val promise: Promise[AsyncResponse] = Promise()
    
    val timeoutTask: TimeoutTask[_] = if (timeout.isFinite()) HttpClient.enableTimeoutTask(promise, timeout) else null
    
    getChannel(host, port, ssl, socksProxy).onComplete {
      case Success(ch) =>
        // Check if the request timed out before we even got a channel
        val doWrite: Boolean = if (null == timeoutTask) true else timeoutTask.synchronized {
          if (timeoutTask.hasRun) {
            ch.close()
            false
          } else {
            timeoutTask.setChannel(ch)
            true
          }
        }
        
        if (doWrite) ch.writeAndFlush(NettyHttpClientPipelineHandler.URIRequestAndPromise(uri, fixedRequest, promise))
      
      case Failure(ex) =>
        promise.tryFailure(ex)
    }
    
    // Only add this handler if we at least have a logging level of "info"
    if (logger.isInfoEnabled) promise.future.onComplete {
      case Success(response) => if (logger.isTraceEnabled) logger.trace(s"Received Response for: ${request.method.name} $url\n$traceSep\n$response\n$traceSep\n")
                                else logger.info(s"Received ${response.status.code} Response for: ${request.method.name} $url")
      case Failure(ex)       => logger.warn(s"Request Failed: ${request.method.name} $url", ex)
    }
    
    promise.future
  }
  
  private def makeNewChannel(p: EndPoint)(pool: ChannelPool): Future[Channel] = makeNewChannel(p.host, p.port, p.ssl, p.socksProxy)(pool)
  
  private def makeNewChannel(host: String, port: Int, ssl: Boolean, socksProxy: Option[(String, Int)] = None)(pool: ChannelPool): Future[Channel] = {
    val promise: Promise[Channel] = Promise()
    
    val connectFuture: ChannelFuture = socksProxy match {
      case Some((socksHost, socksPort)) => client.connect(socksHost, socksPort)
      case None                         => client.connect(host, port)
    }
    
    val ch: Channel = connectFuture.channel()
    
    if (defaultConnectTimeout.isFinite()) HttpClient.enableTimeoutTask(promise, defaultConnectTimeout)
    
    promise.future.onComplete {
      case Success(_)  => 
      // If the connect promise fails (e.g. we timeout) then make sure we close the channel
      case Failure(ex) => ch.close()
    }
    
    if (socksProxy.isDefined) {
      import NettyHttpClientPipelineHandler.{SOCKSInit, SOCKSAuth, SOCKSConnect}
      import scala.collection.JavaConverters._
      
      
      val socksInitPromise: Promise[SocksInitResponse] = Promise()
      //val socksAuthPromise: Promise[SocksAuthResponse] = Promise()
      val socksConnectPromise: Promise[SocksCmdResponse] = Promise()   

      val addrType: SocksAddressType = if (IP.isValid(host)) SocksAddressType.IPv4 else SocksAddressType.DOMAIN
      
      // Once connected we need to tell the Socks Proxy who we want to connect to
      connectFuture.onComplete{
        case Failure(ex) => socksConnectPromise.tryFailure(ex)
        case Success(_) =>
          ch.pipeline().addFirst("socks_encoder", new SocksMessageEncoder)
          ch.pipeline().addFirst("socks_decoder", new SocksInitResponseDecoder)
          
          ch.writeAndFlush(SOCKSInit(new SocksInitRequest(List(SocksAuthScheme.NO_AUTH).asJava), socksInitPromise))
          
          socksInitPromise.future.onComplete {
            case Failure(ex) => socksConnectPromise.tryFailure(ex)
            case Success(initResponse) =>
              if (initResponse.authScheme != SocksAuthScheme.NO_AUTH) socksConnectPromise.tryFailure(new IOException("Invalid AUTH_SCHEME: "+initResponse.authScheme)) else {
                ch.pipeline().addFirst("socks_decoder", new SocksCmdResponseDecoder)
                ch.writeAndFlush(SOCKSConnect(new SocksCmdRequest(SocksCmdType.CONNECT, addrType, host, port), socksConnectPromise))
              }
          }
      }

      socksConnectPromise.future.onComplete{
        case Success(cmdResponse) => 
          ch.pipeline().remove("socks_encoder")
          Try{ ch.pipeline().remove("socks_decoder") }
          
          if (cmdResponse.cmdStatus() == SocksCmdStatus.SUCCESS) {
            if (!promise.trySuccess(ch)) ch.close()             
          } else {
            promise.tryFailure(new Exception("SOCKS Proxy Failure: "+cmdResponse.cmdStatus.name))
            ch.close()
          }
          
        case Failure(ex) => promise.tryFailure(ex)
      }
      
    } else {
      connectFuture.onComplete {
        case Success(_) => if (!promise.trySuccess(ch)) ch.close()
        case Failure(ex) => promise.tryFailure(ex)
      }
    }
    
    if (null != pool ) promise.future.map{ ch: Channel =>
      NettyHttpClientPipelineHandler.setChannelPool(ch, pool)
      ch
    } else {
      promise.future
    }
  }
  
  private[this] val allChannels: ChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  
  private[this] val client: Bootstrap = makeBootstrap(ssl = false)
  //private[this] val sslClient: Bootstrap = makeBootstrap(ssl = true)
  
  private def makeBootstrap(ssl: Boolean): Bootstrap = {
    val b = new Bootstrap
    b.group(workerGroup)
    b.channel(classOf[NioSocketChannel])
    b.option[java.lang.Boolean](ChannelOption.AUTO_READ, false) // ChannelHandlerContext.read() must be explicitly called when we want a message read
    b.handler(new ChannelInitializer[SocketChannel] {
       def initChannel(ch: SocketChannel): Unit = {
         val p: ChannelPipeline = ch.pipeline()

         if (ssl) {
           // TODO: implement this
           val engine: Nothing = ???
           p.addLast("ssl", new SslHandler(engine))
         }
         
         p.addLast("httpcodec",     new HttpClientCodec())
         //p.addLast("decompressor",  new NettyContentDecompressor())
         p.addLast("decompressor",  new io.netty.handler.codec.http.HttpContentDecompressor())
         //p.addLast("chunkedWriter", new ChunkedWriteHandler())
         p.addLast("handler",       new NettyHttpClientPipelineHandler(allChannels, executionContext))
         
       }
    })
    b
  }
  
  if (useConnectionPool) HttpClient.enableIdleTask(this)
}
