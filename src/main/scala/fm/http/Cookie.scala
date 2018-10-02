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
package fm.http

import io.netty.handler.codec.http.cookie.{Cookie => NettyCookie, ClientCookieDecoder => NettyClientCookieDecoder, ServerCookieDecoder => NettyServerCookieDecoder, DefaultCookie => NettyDefaultCookie}
import scala.collection.JavaConverters._
import scala.util.Try
import fm.common.Implicits._

object Cookie {
  
  /**
   * Decodes a Cookie Header into a Set of Cookies
   */
  def parseCookieHeader(s: String): Vector[Cookie] = if (s.isNullOrBlank) Vector.empty else NettyServerCookieDecoder.LAX.decode(s).asScala.map{ apply }.toVector

  /**
   * Exception-Safe version of apply
   */
  def tryParseCookieHeader(s: String): Vector[Cookie] = Try{ parseCookieHeader(s) }.toOption.getOrElse{ Vector.empty }

  /**
   * Decodes a Set-Cookie Header into a Set of Cookies
   */
  def parseSetCookieHeader(s: String): Option[Cookie] = if (s.isNullOrBlank) None else Some(apply(NettyClientCookieDecoder.LAX.decode(s)))

  /**
   * Exception-Safe version of apply
   */
  def tryParseSetCookieHeader(s: String): Option[Cookie] = Try{ parseSetCookieHeader(s) }.toOption.flatten
  
  /**
   * Convert a NettyCookie into our Immutable Cookie
   */
  def apply(netty: NettyCookie): Cookie = Cookie(
    name = netty.name(),
    value = netty.value(),
    maxAge = Some(netty.maxAge()).filterNot{ _ === Long.MinValue },
    path = Option(netty.path()),
    domain = Option(netty.domain()),
    isSecure = netty.isSecure(),
    isHttpOnly = netty.isHttpOnly()
  )
}

final case class Cookie(
  name: String,
  value: String,
  maxAge: Option[Long] = None,
  path: Option[String] = None,
  domain: Option[String] = None,
  isSecure: Boolean = false,
  isHttpOnly: Boolean = false
) {
  
  def toNettyCookie: NettyCookie = {
    val c = new NettyDefaultCookie(name, value)
    maxAge.foreach{ c.setMaxAge }
    path.foreach{ c.setPath }
    domain.foreach{ c.setDomain }
    c.setSecure(isSecure)
    c.setHttpOnly(isHttpOnly)
    c
  }
  
}