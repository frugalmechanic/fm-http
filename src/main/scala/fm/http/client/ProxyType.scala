package fm.http.client

import io.netty.channel.ChannelHandler
import io.netty.handler.proxy.{HttpProxyHandler, Socks4ProxyHandler, Socks5ProxyHandler}

object ProxyType {
  case object Socks4a extends ProxyType {
    def makeChannelHandler(options: ProxyOptions): ChannelHandler = {
      new Socks4ProxyHandler(options.socketAddress, options.user.orNull)
    }
  }

  case object Socks5 extends ProxyType {
    def makeChannelHandler(options: ProxyOptions): ChannelHandler = {
      new Socks5ProxyHandler(options.socketAddress, options.user.orNull, options.pass.orNull)
    }
  }

  case object HTTP extends ProxyType {
    def makeChannelHandler(options: ProxyOptions): ChannelHandler = {
      new HttpProxyHandler(options.socketAddress, options.user.orNull, options.pass.orNull)
    }
  }
}

sealed trait ProxyType {
  def makeChannelHandler(options: ProxyOptions): ChannelHandler
}
