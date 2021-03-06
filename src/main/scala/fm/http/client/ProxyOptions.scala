package fm.http.client

import fm.common.IP
import io.netty.channel.ChannelHandler
import java.net.{InetSocketAddress, SocketAddress}

final case class ProxyOptions(
  proxyType: ProxyType,
  host: String,
  port: Int,
  user: Option[String],
  pass: Option[String]
) {
  def makeChannelHandler: Option[ChannelHandler] = proxyType.makeChannelHandler(this)
  def passFullURLInRequest: Boolean = proxyType.passFullURLInRequest
  def bootstrapShouldConnectToProxy: Boolean = proxyType.bootstrapShouldConnectToProxy

  def socketAddress: SocketAddress = {
    if (IP.isValid(host)) new InetSocketAddress(IP(host).inetAddress, port)
    else new InetSocketAddress(host, port)
  }
}
