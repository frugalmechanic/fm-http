package fm.http.client

import io.netty.channel.ChannelHandler
import io.netty.handler.proxy.{HttpProxyHandler, Socks4ProxyHandler, Socks5ProxyHandler}

object ProxyType {
  case object Socks4a extends ProxyType {
    def makeChannelHandler(options: ProxyOptions): Option[ChannelHandler] = {
      Some(new Socks4ProxyHandler(options.socketAddress, options.user.orNull))
    }
  }

  case object Socks5 extends ProxyType {
    def makeChannelHandler(options: ProxyOptions): Option[ChannelHandler] = {
      Some(new Socks5ProxyHandler(options.socketAddress, options.user.orNull, options.pass.orNull))
    }
  }

  case object HTTPConnect extends ProxyType {
    def makeChannelHandler(options: ProxyOptions): Option[ChannelHandler] = {
      if (options.user.isDefined && options.pass.isDefined) {
        Some(new HttpProxyHandler(options.socketAddress, options.user.get, options.pass.get))
      } else {
        Some(new HttpProxyHandler(options.socketAddress))
      }
    }
  }

  case object HTTP extends ProxyType {
    def makeChannelHandler(options: ProxyOptions): Option[ChannelHandler] = None
    override def passFullURLInRequest: Boolean = true
    override def bootstrapShouldConnectToProxy: Boolean = true
  }
}

sealed trait ProxyType {
  def makeChannelHandler(options: ProxyOptions): Option[ChannelHandler]
  def passFullURLInRequest: Boolean = false
  def bootstrapShouldConnectToProxy: Boolean = false
}
