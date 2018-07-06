package fm.http

import fm.common.Implicits._
import fm.common.Logging
import io.netty.channel.EventLoopGroup
import io.netty.channel.epoll.{Epoll, EpollEventLoopGroup, EpollServerSocketChannel, EpollSocketChannel}
import io.netty.channel.kqueue.{KQueue, KQueueEventLoopGroup, KQueueServerSocketChannel, KQueueSocketChannel}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.{ServerSocketChannel, SocketChannel}
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import java.util.concurrent.ThreadFactory

private[http] object NativeHelpers extends Logging {
  private def getBooleanSystemProperty(key: String): Option[Boolean] = {
    Option(System.getProperty(key)).flatMap{ _.parseBoolean }
  }

  // Disabled by default
  private def defaultUseNativeTransport: Boolean = false

  // Setting this will enable client and server native transport unless overridden below
  private def useNativeTransport: Option[Boolean] = getBooleanSystemProperty("fm.http.use_native_transport")

  // Setting either of these will override fm.http.use_native_transport
  private def useServerNativeTransport: Option[Boolean] = getBooleanSystemProperty("fm.http.server.use_native_transport")
  private def useClientNativeTransport: Option[Boolean] = getBooleanSystemProperty("fm.http.client.use_native_transport")

  private def shouldUseNative(initial: Option[Boolean]): Boolean = {
    initial orElse useNativeTransport getOrElse defaultUseNativeTransport
  }

  def makeServerEventLoopGroup(nThreads: Int, threadFactory: ThreadFactory): EventLoopGroup = {
    makeEventLoopGroup("Server", shouldUseNative(useServerNativeTransport), nThreads, threadFactory)
  }

  def makeClientEventLoopGroup(nThreads: Int, threadFactory: ThreadFactory): EventLoopGroup = {
    makeEventLoopGroup("Client", shouldUseNative(useClientNativeTransport), nThreads, threadFactory)
  }

  private def makeEventLoopGroup(label: String, useNative: Boolean, nThreads: Int, threadFactory: ThreadFactory): EventLoopGroup = {
    val res: EventLoopGroup =
      if (useNative && Epoll.isAvailable) new EpollEventLoopGroup(nThreads, threadFactory)
      else if (useNative && KQueue.isAvailable) new KQueueEventLoopGroup(nThreads, threadFactory)
      else new NioEventLoopGroup(nThreads, threadFactory)

    logger.info(s"Using $label EventLoopGroup: $res")

    res
  }

  def serverSocketChannelClass: Class[_ <: ServerSocketChannel] = {
    val useNative: Boolean = shouldUseNative(useServerNativeTransport)

    val res: Class[_ <: ServerSocketChannel] =
      if (useNative && Epoll.isAvailable) classOf[EpollServerSocketChannel]
      else if (useNative && KQueue.isAvailable) classOf[KQueueServerSocketChannel]
      else classOf[NioServerSocketChannel]

    logger.info(s"Using ServerSocketChannel: $res")

    res
  }

  def socketChannelClass: Class[_ <: SocketChannel] = {
    val useNative: Boolean = shouldUseNative(useClientNativeTransport)

    val res: Class[_ <: SocketChannel] =
      if (useNative && Epoll.isAvailable) classOf[EpollSocketChannel]
      else if (useNative && KQueue.isAvailable) classOf[KQueueSocketChannel]
      else classOf[NioSocketChannel]

    logger.info(s"Using SocketChannel $res")

    res
  }
}
