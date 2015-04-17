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

import io.netty.handler.codec.http.{Cookie => NettyCookie, CookieDecoder => NettyCookieDecoder, DefaultCookie => NettyDefaultCookie}
import scala.collection.JavaConverters._
import scala.util.Try
import fm.common.Implicits._

object Cookie {
  /**
   * Decodes a Cookie or Set-Cookie Header into a Set of Cookies
   */
  def parse(s: String): Vector[Cookie] = if (s.isBlank) Vector.empty else NettyCookieDecoder.decode(s).asScala.map{ apply }.toVector

  /**
   * Exception-Safe version of apply
   */
  def tryParse(s: String): Vector[Cookie] = Try{ parse(s) }.toOption.getOrElse{ Vector.empty }
  
  /**
   * Convert a NettyCookie into our Immutable Cookie
   */
  def apply(netty: NettyCookie): Cookie = Cookie(
    name = netty.getName(),
    value = netty.getValue(),
    maxAge = Some(netty.getMaxAge()).filterNot{ _ === Long.MinValue },
    path = Option(netty.getPath()),
    domain = Option(netty.getDomain()),
    isSecure = netty.isSecure(),
    isHttpOnly = netty.isHttpOnly(),
    ports = netty.getPorts().asScala.toSet.map{ i: Integer => i.intValue() },
    version = netty.getVersion(),
    isDiscard = netty.isDiscard(),
    comment = Option(netty.getComment()),
    commentUrl = Option(netty.getCommentUrl())
  )
}

final case class Cookie(
  name: String,
  value: String,
  maxAge: Option[Long] = None,
  path: Option[String] = None,
  domain: Option[String] = None,
  isSecure: Boolean = false,
  isHttpOnly: Boolean = false,
  ports: Set[Int] = Set.empty,
  version: Int = 0,
  isDiscard: Boolean = false,
  comment: Option[String] = None,
  commentUrl: Option[String] = None
) {
  
  def toNettyCookie: NettyCookie = {
    val c = new NettyDefaultCookie(name, value)
    maxAge.foreach{ c.setMaxAge }
    path.foreach{ c.setPath }
    domain.foreach{ c.setDomain }
    c.setSecure(isSecure)
    c.setHttpOnly(isHttpOnly)
    c.setPorts(ports.toSeq: _*)
    c.setVersion(version)
    c.setDiscard(isDiscard)
    comment.foreach{ c.setComment }
    commentUrl.foreach{ c.setCommentUrl }
    c
  }
  
}