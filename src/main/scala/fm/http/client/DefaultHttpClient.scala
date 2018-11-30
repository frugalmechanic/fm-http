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
import fm.common.{Logging, URI, URL}
import fm.http._
import io.netty.bootstrap.Bootstrap
import io.netty.channel._
import io.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import io.netty.channel.socket.SocketChannel
import io.netty.handler.codec.http.{HttpClientCodec, HttpMethod}
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.handler.ssl.{SslContext, SslContextBuilder}
import io.netty.util.concurrent.GlobalEventExecutor
import java.util.concurrent.{ConcurrentHashMap, TimeoutException}
import java.nio.charset.Charset
import java.lang.ref.WeakReference
import java.net.MalformedURLException
import javax.net.ssl.TrustManagerFactory
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success}


/**
 * This holds a single copy of the EventLoopGroup / NettyExecutionContext
 */
object DefaultHttpClient extends Logging {
  import HttpClient.{executionContext, timer}
  
//  def close(): Unit = {
//    workerGroup.shutdownGracefully(1, 15, TimeUnit.SECONDS)
//  }
  
  private val workerGroup: EventLoopGroup = {
    NativeHelpers.makeClientEventLoopGroup(0, new ThreadFactory("fm-http-client-worker", daemon = true))
  }
    
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
   
  private case class EndPoint(host: String, port: Int, ssl: Boolean, proxy: Option[ProxyOptions]) {
    def prettyString: String = {
      val scheme: String = if (ssl) "https" else "http"
      val proxyStr: String = proxy.map{ p: ProxyOptions => s" via ${p.host}:${p.port}" }.getOrElse("")
      s"${scheme}://$host:$port$proxyStr"
    }
  }
  
  private val IdleCheckDuration: FiniteDuration = 5.seconds
  
  private def enableTimeoutTask[T](promise: Promise[T], duration: Duration): TimeoutTask[T] = {
    val task: TimeoutTask[T] = new TimeoutTask(new WeakReference(promise))
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
  
  private def enableIdleTask(client: DefaultHttpClient): Unit = {
    timer.schedule(new ConnectionIdleTask(new WeakReference(client)), IdleCheckDuration)
  }
  
  private class ConnectionIdleTask(ref: WeakReference[DefaultHttpClient]) extends Runnable {
    def run(): Unit = {
      val client: DefaultHttpClient = ref.get()
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

  @deprecated("proxy is now a required constructor parameter", "0.25.0") def apply(
    defaultMaxLength: Long,
    defaultHeaders: Headers,
    useConnectionPool: Boolean,  // Should we re-use connections? (Use HTTP Keep Alive?)
    maxConnectionsPerHost: Int,  // Only applies if useConnectionPool is true
    maxRequestQueuePerHost: Int, // Only applies if useConnectionPool is true
    maxConnectionIdleDuration: FiniteDuration,
    defaultResponseTimeout: Duration, // The maximum time to wait for a Response
    defaultConnectTimeout: Duration, // The maximum time to wait to connect to a server
    defaultCharset: Charset, // The default charset to use (if none is specified in the response) when converting responses to strings
    followRedirects: Boolean, // Should 301/302 redirects be followed for GET or HEAD requests?
    maxRedirectCount: Int, // The maximum number of 301/302 redirects to follow for a GET or HEAD request
    disableSSLCertVerification: Boolean, // Do not verify SSL certs (SHOULD NOT USE IN PRODUCTION)
    useExpect100Continue: Boolean,
  ): DefaultHttpClient = DefaultHttpClient(
    None,
    defaultMaxLength,
    defaultHeaders,
    useConnectionPool,
    maxConnectionsPerHost,
    maxRequestQueuePerHost,
    maxConnectionIdleDuration,
    defaultResponseTimeout,
    defaultConnectTimeout,
    defaultCharset,
    followRedirects,
    maxRedirectCount,
    disableSSLCertVerification,
    useExpect100Continue,
  )
}

final case class DefaultHttpClient(
  proxy: Option[ProxyOptions],
  defaultMaxLength: Long,
  defaultHeaders: Headers,
  useConnectionPool: Boolean,  // Should we re-use connections? (Use HTTP Keep Alive?)
  maxConnectionsPerHost: Int,  // Only applies if useConnectionPool is true
  maxRequestQueuePerHost: Int, // Only applies if useConnectionPool is true
  maxConnectionIdleDuration: FiniteDuration,
  defaultResponseTimeout: Duration, // The maximum time to wait for a Response
  defaultConnectTimeout: Duration, // The maximum time to wait to connect to a server
  defaultCharset: Charset, // The default charset to use (if none is specified in the response) when converting responses to strings
  followRedirects: Boolean, // Should 301/302 redirects be followed for GET or HEAD requests?
  maxRedirectCount: Int, // The maximum number of 301/302 redirects to follow for a GET or HEAD request
  disableSSLCertVerification: Boolean, // Do not verify SSL certs (SHOULD NOT USE IN PRODUCTION)
  useExpect100Continue: Boolean, // Attempt to send an Expect: 100 Continue on requests with a Content-Length sent before sending the content
) extends HttpClient with Logging {
  import DefaultHttpClient.{EndPoint, TimeoutTask, workerGroup}
  
  require(maxConnectionsPerHost > 0, "maxConnectionsPerHost must be > 0")
  require(maxRedirectCount >= 0, "maxRedirectCount must be >= 0")

  /**
   * Execute a Request returning the AsyncResponse
   */
  def execute(r: Request, timeout: Duration): Future[AsyncResponse] = {
    if (followRedirects && (r.method === HttpMethod.GET || r.method === HttpMethod.HEAD)) {
      // Handle 301/302 redirects
      executeWithRedirects(r, timeout, 0)
    } else {
      // Don't handle redirects
      if (useExpect100Continue) executeWithExpectContinue(r, timeout)
      else execute0(r.url, r, timeout, false)
    }
  }
  
  private def executeWithRedirects(r: Request, timeout: Duration, redirectCount: Int): Future[AsyncResponse] = {
    if (redirectCount > maxRedirectCount) return Future.failed{ new TooManyRedirectsException(s"Too many redirects ($redirectCount) for request $r") }

    val f: Future[AsyncResponse] = {
      if (useExpect100Continue) executeWithExpectContinue(r, timeout)
      else execute0(r.url, r, timeout, false)
    }

    f.flatMap { response: AsyncResponse =>
      if (response.status === Status.MOVED_PERMANENTLY || response.status === Status.FOUND) {
        response.headers.location.flatMap{ URI.get }.map{ location: URI =>
          
          // Allow for a relative URL
          val redirectURL: URL = if (location.scheme.isDefined) location.toURL() else location.copy(
            scheme = location.scheme orElse r.url.scheme,
            host = location.host orElse r.url.host,
            port = location.port orElse (if (location.host.isDefined) None else r.url.port)
          ).toURL
          
          response.close()
          
          val newRequest: Request = FullRequest(HttpMethod.GET, redirectURL, r.headers)
          executeWithRedirects(newRequest, timeout, redirectCount + 1)
        }.getOrElse{ Future.successful(response) }
        
      } else {
        Future.successful(response)
      }
    }
  }

  private def executeWithExpectContinue(r: Request, timeout: Duration): Future[AsyncResponse] = {
    execute0(r.url, r, timeout, true).flatMap { response: AsyncResponse =>
      if (response.status === Status.CONTINUE) {
        response.close()
        execute0(r.url, r, timeout, false)
      } else Future.successful(response)
    }
  }
  
  def close(): Unit = {
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
  private def getChannel(host: String, port: Int, ssl: Boolean, proxy: Option[ProxyOptions]): Future[Channel] = {
    val endPoint: EndPoint = EndPoint(host, port, ssl, proxy)
    
    if (useConnectionPool) endpointMap.synchronized {
      val poolRef: WeakReference[ChannelPool] = endpointMap.get(endPoint)
      
      var pool: ChannelPool = if (null != poolRef) poolRef.get else null
      
      if (null == pool) {
        pool = new ChannelPool(endPoint.prettyString, makeNewChannel(endPoint), limit = maxConnectionsPerHost, maxQueueSize = maxRequestQueuePerHost, maxIdleMillis = maxConnectionIdleDuration.toMillis)
        endpointMap.put(endPoint, new WeakReference(pool))
      }
      
      pool.checkout()
    } else {
      makeNewChannel(endPoint)(null)
    }
  }
  
  private[this] val traceSep: String = "======================================================================================================================="
  
  private def execute0(url: URL, request: Request, timeout: Duration, send100ContinueExpected: Boolean): Future[AsyncResponse] = {
    if (logger.isTraceEnabled) logger.trace(s"Sending Request:\n$traceSep\n$request\n$traceSep\n")
    else logger.info(s"${request.method.name} $url")
    
    val scheme: String = url.scheme.getOrElse{ return Future.failed(new MalformedURLException("Missing Scheme")) }
    val host: String = url.host.getOrElse{ return Future.failed(new MalformedURLException("Missing Host")) }
    
    val defaultPort: Int = scheme match {
      case "http" => 80
      case "https" => 443
      case _ => return Future.failed(new MalformedURLException("We only support 'http' and 'https' as schemes"))
    }

    val port: Int = url.port.getOrElse{ defaultPort }
    
    val ssl: Boolean = scheme === "https"
    
    // Add a "Host:" header if its missing
    val fixedRequest: Request = if (request.headers.host.isEmpty) {
      val newHeaders = request.headers.toMutableHeaders
      newHeaders.host = host
      request.withHeaders(newHeaders)
    } else request
    
    // This if the URI part of the request:  /foo/bar?param=value
    val uri: String = request.requestURI
    
    val promise: Promise[AsyncResponse] = Promise()
    
    val timeoutTask: TimeoutTask[_] = if (timeout.isFinite()) DefaultHttpClient.enableTimeoutTask(promise, timeout) else null
    
    getChannel(host, port, ssl, proxy).onComplete {
      case Success(ch) =>
        if (logger.isTraceEnabled) logger.trace("Success for getChannel() => "+ch+"  URI: "+uri)
        
        // Check if the request timed out before we even got a channel
        val doWrite: Boolean = if (null == timeoutTask) true else timeoutTask.synchronized {
          if (timeoutTask.hasRun) {
            if (logger.isTraceEnabled) logger.trace("Got channel but timeoutTask has already run.  Channel: "+ch)
            ch.close()
            false
          } else {
            timeoutTask.setChannel(ch)
            true
          }
        }
        
        if (doWrite) {
          if (logger.isTraceEnabled) logger.trace(s"ch.writeAndFlush to channel: $ch for $host:$port$uri")
          ch.writeAndFlush(NettyHttpClientPipelineHandler.URIRequestAndPromise(uri, fixedRequest, promise, send100ContinueExpected))
        }
      
      case Failure(ex) =>
        if (logger.isTraceEnabled) logger.trace("Failed to getChannel()")
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
  
  private def makeNewChannel(p: EndPoint)(pool: ChannelPool): Future[Channel] = makeNewChannel(p.host, p.port, p.ssl, p.proxy)(pool)
  
  private def makeNewChannel(host: String, port: Int, ssl: Boolean, proxy: Option[ProxyOptions])(pool: ChannelPool): Future[Channel] = {
    val promise: Promise[Channel] = Promise()
    
    val bootstrap: Bootstrap = if (ssl) httpsBootstrap(host, port) else httpBootstrap

    val connectFuture: ChannelFuture = bootstrap.connect(host, port)
    
    val ch: Channel = connectFuture.channel()
    
    if (defaultConnectTimeout.isFinite()) DefaultHttpClient.enableTimeoutTask(promise, defaultConnectTimeout)
    
    promise.future.onComplete {
      case Success(_)  => 
      // If the connect promise fails (e.g. we timeout) then make sure we close the channel
      case Failure(ex) => ch.close()
    }

    connectFuture.onComplete {
      case Success(_) => if (!promise.trySuccess(ch)) ch.close()
      case Failure(ex) => promise.tryFailure(ex)
    }
    
    if (null != pool) promise.future.map{ ch: Channel =>
      NettyHttpClientPipelineHandler.setChannelPool(ch, pool)
      ch
    } else {
      promise.future
    }
  }
  
  private[this] val allChannels: ChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  
  // We can re-use a single Bootstrap instance for HTTP connections
  private[this] val httpBootstrap: Bootstrap = makeBootstrap(false, "", 0)
  
  // We create Bootstrap instances for HTTPS on the fly since they include the host/port
  private[this] def httpsBootstrap(host: String, port: Int): Bootstrap = makeBootstrap(true, host, port)
  
  private[this] val sslCtx: SslContext = {
    val trustManager: TrustManagerFactory =
      if (disableSSLCertVerification) InsecureTrustManagerFactory.INSTANCE
      else null // null means to use system default

    SslContextBuilder.forClient().trustManager(trustManager).build()
  }
  
  private def makeBootstrap(ssl: Boolean, host: String, port: Int): Bootstrap = {
    val b: Bootstrap = new Bootstrap()
    b.group(workerGroup)
    b.channel(NativeHelpers.socketChannelClass)
    b.option[java.lang.Boolean](ChannelOption.AUTO_READ, false) // ChannelHandlerContext.read() must be explicitly called when we want a message read
    b.handler(new ChannelInitializer[SocketChannel] {
       def initChannel(ch: SocketChannel): Unit = {
         val p: ChannelPipeline = ch.pipeline()

         // If we are using a proxy then make sure that gets added first
         proxy.foreach{ options: ProxyOptions => p.addFirst(options.makeChannelHandler) }

         if (ssl) {
           p.addLast("ssl", sslCtx.newHandler(ch.alloc(), host, port))
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
  
  if (useConnectionPool) DefaultHttpClient.enableIdleTask(this)
}
