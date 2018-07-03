package fm.http

import io.netty.channel.EventLoopGroup
import io.netty.channel.epoll.{Epoll, EpollEventLoopGroup, EpollServerSocketChannel, EpollSocketChannel}
import io.netty.channel.kqueue.{KQueue, KQueueEventLoopGroup, KQueueServerSocketChannel, KQueueSocketChannel}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.{ServerSocketChannel, SocketChannel}
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import java.util.concurrent.ThreadFactory

private[http] object NativeHelpers {
  def makeEventLoopGroup(nThreads: Int, threadFactory: ThreadFactory): EventLoopGroup = {
    if (Epoll.isAvailable) new EpollEventLoopGroup(nThreads, threadFactory)
    else if (KQueue.isAvailable) new KQueueEventLoopGroup(nThreads, threadFactory)
    else new NioEventLoopGroup(nThreads, threadFactory)
  }

  def serverSocketChannelClass: Class[_ <: ServerSocketChannel] = {
    if (Epoll.isAvailable) classOf[EpollServerSocketChannel]
    else if (KQueue.isAvailable) classOf[KQueueServerSocketChannel]
    else classOf[NioServerSocketChannel]
  }

  def socketChannelClass: Class[_ <: SocketChannel] = {
    if (Epoll.isAvailable) classOf[EpollSocketChannel]
    else if (KQueue.isAvailable) classOf[KQueueSocketChannel]
    else classOf[NioSocketChannel]
  }
}
