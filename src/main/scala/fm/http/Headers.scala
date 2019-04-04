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

import fm.common.Implicits._
import fm.common.{Base64, ImmutableDate, IndexedSeqProxy}
import java.nio.charset.StandardCharsets
import java.util.Date
import io.netty.handler.codec.http.{DefaultHttpHeaders, EmptyHttpHeaders, HttpHeaderNames, HttpHeaders}
import io.netty.handler.codec.http.cookie.{ClientCookieEncoder, ServerCookieEncoder}
import io.netty.util.AsciiString
import java.time._
import java.time.format.DateTimeFormatter
import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.matching.Regex

object Headers {  
  val empty: Headers = ImmutableHeaders(EmptyHttpHeaders.INSTANCE)
  
  def apply(headerValues: (CharSequence, Any)*): ImmutableHeaders = {
    val headers: MutableHeaders = MutableHeaders()
    headers.withHeaders(headerValues:_*)
    
    // Convert to immutable headers - this is safe since we know we won't be touching the MutableHeaders again
    ImmutableHeaders(headers.nettyHeaders)
  }
  
  def noCache(headerValues: (CharSequence, Any)*): ImmutableHeaders = {
    val headers: MutableHeaders = MutableHeaders()
    headers.withNoCache
    headers.withHeaders(headerValues:_*)
    
    // Convert to immutable headers - this is safe since we know we won't be touching the MutableHeaders again
    ImmutableHeaders(headers.nettyHeaders)
  }

  private val GMT: ZoneId = ZoneId.of("GMT")
  
  private[http] def formatDate(): String = formatDate(OffsetDateTime.now)
  private[http] def formatDate(millis: Long): String = formatDate(Instant.ofEpochMilli(millis))
  private[http] def formatDate(date: Date): String = formatDate(date.toInstant)
  private[http] def formatDate(date: ImmutableDate): String = formatDate(date.toInstant)
  private[http] def formatDate(date: OffsetDateTime): String = formatDate(date.toZonedDateTime)
  private[http] def formatDate(date: Instant): String = formatDate(ZonedDateTime.ofInstant(date, GMT))
  private[http] def formatDate(date: LocalDateTime): String = formatDate(date.atZone(ZoneId.systemDefault()))

  private[http] def formatDate(date: ZonedDateTime): String = {
    date.withZoneSameInstant(GMT).format(DateTimeFormatter.RFC_1123_DATE_TIME)
  }

//  private[http] def parseZonedDateTime(s: String): Option[ZonedDateTime] = {
//    if (s.isNullOrBlank) return None
//    val cleaned: String = if(s.contains(";")) s.substring(0, s.indexOf(";")) else s
//    Try{ ZonedDateTime.parse(cleaned, DateTimeFormatter.RFC_1123_DATE_TIME) }.toOption
//  }

  private[http] def parseImmutableDate(s: String): Option[ImmutableDate] = {
    if (s.isNullOrBlank) return None
    val cleaned: String = if(s.contains(";")) s.substring(0, s.indexOf(";")) else s
    Try{ ZonedDateTime.parse(cleaned, DateTimeFormatter.RFC_1123_DATE_TIME) }.toOption.map{ _.toInstant }.map{ ImmutableDate(_) }
  }

  // A very very simple Regex for parsing the filename from the Content-Disposition Header
  private[http] val ContentDispositionAttachmentFileNamePattern: Regex = """attachment; filename="(.*)"""".r

  private[http] def parseContentDispositionAttachmentFileName(s: String): Option[String] = s match {
    case ContentDispositionAttachmentFileNamePattern(name) => name.toBlankOption
    case _ => None
  }

  private[http] object NonStandardNames {
    private implicit def toAsciiString(s: String): AsciiString = new AsciiString(s)

    //
    // Non-Standard Request Headers
    //
    val DNT: AsciiString = "DNT"
    val X_REQUESTED_WITH: AsciiString = "X-Requested-With"
    val X_FORWARDED_FOR: AsciiString = "X-Forwarded-For"
    val X_FORWARDED_PROTO: AsciiString = "X-Forwarded-Proto"
    val X_SSL: AsciiString = "X-SSL"
   
    //
    // Non-Standard Response Headers
    //
    val X_FRAME_OPTIONS: AsciiString = "X-Frame-Options"
    val X_XSS_PROTECTION: AsciiString = "X-XSS-Protection"
    val X_CONTENT_TYPE_OPTIONS: AsciiString = "X-Content-Type-Options"
    val X_POWERED_BY: AsciiString = "X-Powered-By"
    val X_UA_COMPATIBLE: AsciiString = "X-UA-Compatible"
  }
  
  private val BasicAuthHeader: Regex = """Basic (.+)""".r
  private val BasicAuthSplit: Regex = """(.+):(.*)""".r
  
  private val DigestAuthHeader: Regex = """Digest (.+)""".r
  private val DigestAuthParam: Regex = """(\w+)=(?:"([^"]+)"|([^,]+)),?""".r
  
  /**
   * Given the value of the Authorization header parse the username/password for Basic auth
   */
  def parseBasicAuthorization(value: String): Option[(String,String)] = try {
    value match {
      case BasicAuthHeader(encoded) => new String(Base64.decode(encoded), StandardCharsets.ISO_8859_1) match {
        case BasicAuthSplit(user, pass) => Some((user, pass))
        case _ => None
      }
      case _ => None
    }
  } catch {
    // This will catch Base64 decoding exceptions and just return None
    case ex: Exception => None
  }
  
  def makeBasicAuthorization(user: String, pass: String): String = "Basic "+Base64.encodeBytes((user+":"+pass).getBytes(StandardCharsets.ISO_8859_1))
  
  /**
   * Given the value of the WWW-Authenticate or Authorization headers parse the Digest auth params
   */
  private def parseDigestAuthParams(str: String): Option[Map[String,String]] = {
    str match {
      case DigestAuthHeader(paramsStr) =>
        val params: Map[String, String] = Map(DigestAuthParam.findAllIn(paramsStr).matchData.map{ m => 
            val k: String = m.group(1)
            val v: String = if (null != m.group(2)) m.group(2) else m.group(3)
            (k,v)
          }.toSeq: _*)
       
       Some(params)
       
      case _ => None
    }
  }
  
  /**
   * Create the WWW-Authenticate or Authorization header value given the Digest auth params
   */
  def makeDigestAuthorization(params: Seq[(String,String)]): String = {
    "Digest "+params.map{case (k,v) => k+"=\""+v+"\""}.mkString(", ")
  }
}

sealed trait Headers extends IndexedSeqProxy[(String, String)] {
  import Headers._
  
  private[http] def nettyHeaders: HttpHeaders
  def self: Vector[(String, String)] = nettyHeaders.asScala.toVector.map{ entry => entry.getKey() -> entry.getValue() }
  
  def toMutableHeaders: MutableHeaders = this match {
    case ImmutableHeaders(headers) => MutableHeaders(cloneNettyHeaders())
    case self: MutableHeaders => self
  }
  
  def toImmutableHeaders: ImmutableHeaders = this match {
    case self: ImmutableHeaders => self  
    case MutableHeaders(headers) => ImmutableHeaders(cloneNettyHeaders())
  }
  
  private def cloneNettyHeaders(): HttpHeaders = {
    val h = new DefaultHttpHeaders
    h.add(nettyHeaders)
    h
  }
  
  /** Adds the passed in headers */
  def withHeaders(headerValues: (CharSequence, Any)*): Headers
  
  /** Add the Cache-Control and Expires headers to prevent caching of this response */
  def withNoCache: Headers
  
  /** These are the client side cookies that are being sent with the request */
  def withCookies(c: Traversable[Cookie]): Headers
  
  /** These are the server side cookies that are being sent with the response */
  def withSetCookies(c: Traversable[Cookie]): Headers
  
  /** These are the client side cookies that are being sent with the request */
  def addCookie(c: Cookie): Headers = {
    withCookies(cookies.filterNot{ _.name === c.name } :+ c)
  }
  
  /** These are the server side cookies that are being sent with the response */
  def addSetCookie(c: Cookie): Headers = {
    withSetCookies(setCookies.filterNot{ _.name === c.name } :+ c)
  }
  
  override def toString: String = self.map{ case (k,v) => s"$k: $v" }.mkString("\n")
  
  def apply(name: CharSequence): String = get(name).getOrElse{ throw new NoSuchElementException("No header value for: "+name) }
  def get(name: CharSequence): Option[String] = Option(nettyHeaders.get(name))
  
  def getAll(name: CharSequence): Vector[String] = nettyHeaders.getAll(name).asScala.toVector
  
  def getBool(name: CharSequence): Option[Boolean] = get(name).flatMap{ _.parseBoolean }
  def getDate(name: CharSequence): Option[ImmutableDate] = get(name).flatMap{ parseImmutableDate }
  def getInt(name: CharSequence): Option[Int] = get(name).flatMap{ _.toIntOption }
  def getLong(name: CharSequence): Option[Long] = get(name).flatMap{ _.toLongOption }
  
  /** A helper to find a client-sent cookie by name */
  def getCookie(name: String): Option[Cookie] = cookies.find{ _.name === name }
  
  /**
   * If the Host header has a port in it (e.g. frugalmechanic.com:8080) then
   * this will strip it out.
   */
  def hostWithoutPort: Option[String] = host.map { h: String =>
    val idx = h.indexOf(":")
    if(-1 === idx) h else h.substring(0, idx)
  }
  
  /**
   * The port from the host header (e.g. frugalmechanic.com:8080)
   */
  def hostPort: Option[Int] = host.flatMap { h: String =>
    val idx = h.indexOf(":")
    if(-1 === idx) None else h.substring(idx + 1).toIntOption
  }
  
  def accept: Option[String] = get(HttpHeaderNames.ACCEPT)
  def acceptCharset: Option[String] = get(HttpHeaderNames.ACCEPT_CHARSET)
  def acceptEncoding: Option[String] = get(HttpHeaderNames.ACCEPT_ENCODING)
  def acceptLanguage: Option[String] = get(HttpHeaderNames.ACCEPT_LANGUAGE)
  def acceptPatch: Option[String] = get(HttpHeaderNames.ACCEPT_PATCH)
  def acceptRanges: Option[String] = get(HttpHeaderNames.ACCEPT_RANGES)
  def accessControlAllowCredentials: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS)
  def accessControlAllowHeaders: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS)
  def accessControlAllowMethods: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS)
  def accessControlAllowOrigin: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN)
  def accessControlExposeHeaders: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_EXPOSE_HEADERS)
  def accessControlMaxAge: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_MAX_AGE)
  def accessControlRequestHeaders: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_REQUEST_HEADERS)
  def accessControlRequestMethod: Option[String] = get(HttpHeaderNames.ACCESS_CONTROL_REQUEST_METHOD)
  def age: Option[Long] = getLong(HttpHeaderNames.AGE)
  def allow: Option[String] = get(HttpHeaderNames.ALLOW)
  def authorization: Option[String] = get(HttpHeaderNames.AUTHORIZATION)
  def cacheControl: Option[String] = get(HttpHeaderNames.CACHE_CONTROL)
  def connection: Option[String] = get(HttpHeaderNames.CONNECTION)
  def contentBase: Option[String] = get(HttpHeaderNames.CONTENT_BASE)
  def contentDisposition: Option[String] = get(HttpHeaderNames.CONTENT_DISPOSITION)
  def contentDispositionAttachmentFileName: Option[String] = contentDisposition.flatMap{ parseContentDispositionAttachmentFileName }
  def contentEncoding: Option[String] = get(HttpHeaderNames.CONTENT_ENCODING)
  def contentLanguage: Option[String] = get(HttpHeaderNames.CONTENT_LANGUAGE)
  def contentLength: Option[Long] = getLong(HttpHeaderNames.CONTENT_LENGTH)
  def contentLocation: Option[String] = get(HttpHeaderNames.CONTENT_LOCATION)
  def contentMD5: Option[String] = get(HttpHeaderNames.CONTENT_MD5)
  def contentRange: Option[String] = get(HttpHeaderNames.CONTENT_RANGE)
  def contentTransferEncoding: Option[String] = get(HttpHeaderNames.CONTENT_TRANSFER_ENCODING)
  def contentType: Option[String] = get(HttpHeaderNames.CONTENT_TYPE)
  
  /** This is the raw value of the client side cookies that are being sent with the request */
  def cookie: Option[String] = get(HttpHeaderNames.COOKIE)
  
  /** These are the client side cookies that are being sent with the request */
  def cookies: Vector[Cookie] = cookie.map{ Cookie.tryParseCookieHeader }.getOrElse{ Vector.empty }
  
  def date: Option[ImmutableDate] = getDate(HttpHeaderNames.DATE)
  def eTag: Option[String] = get(HttpHeaderNames.ETAG)
  def expect: Option[String] = get(HttpHeaderNames.EXPECT)
  def expires: Option[ImmutableDate] = getDate(HttpHeaderNames.EXPIRES)
  def from: Option[String] = get(HttpHeaderNames.FROM)
  def host: Option[String] = get(HttpHeaderNames.HOST)
  def ifMatch: Option[String] = get(HttpHeaderNames.IF_MATCH)
  def ifModifiedSince: Option[ImmutableDate] = getDate(HttpHeaderNames.IF_MODIFIED_SINCE)
  def ifNoneMatch: Option[String] = get(HttpHeaderNames.IF_NONE_MATCH)
  def ifRange: Option[String] = get(HttpHeaderNames.IF_RANGE)
  def ifUnmodifiedSince: Option[ImmutableDate] = getDate(HttpHeaderNames.IF_UNMODIFIED_SINCE)
  def lastModified: Option[ImmutableDate] = getDate(HttpHeaderNames.LAST_MODIFIED)
  def location: Option[String] = get(HttpHeaderNames.LOCATION)
  def maxForwards: Option[String] = get(HttpHeaderNames.MAX_FORWARDS)
  def origin: Option[String] = get(HttpHeaderNames.ORIGIN)
  def pragma: Option[String] = get(HttpHeaderNames.PRAGMA)
  def proxyAuthencate: Option[String] = get(HttpHeaderNames.PROXY_AUTHENTICATE)
  def proxyAuthorization: Option[String] = get(HttpHeaderNames.PROXY_AUTHORIZATION)
  def range: Option[String] = get(HttpHeaderNames.RANGE)
  def referer: Option[String] = get(HttpHeaderNames.REFERER)
  def retryAfter: Option[String] = get(HttpHeaderNames.RETRY_AFTER)
  def secWebSocketAccept: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_ACCEPT)
  def secWebSocketKey: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_KEY)
  def secWebSocketKey1: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_KEY1)
  def secWebSocketKey2: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_KEY2)
  def secWebSocketLocation: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_LOCATION)
  def secWebSocketOrigin: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_ORIGIN)
  def secWebSocketProtocol: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_PROTOCOL)
  def secWebSocketVersion: Option[String] = get(HttpHeaderNames.SEC_WEBSOCKET_VERSION)
  def server: Option[String] = get(HttpHeaderNames.SERVER)
  
  /** This is the raw value of the server side cookies that are being sent with the response */
  def setCookie: Vector[String] = getAll(HttpHeaderNames.SET_COOKIE)
  
  /** These are the server side cookies that are being sent with the response */
  def setCookies: Vector[Cookie] = setCookie.flatMap{ Cookie.tryParseSetCookieHeader }

  def setCookie2: Vector[String] = getAll(HttpHeaderNames.SET_COOKIE2)
  def te: Option[String] = get(HttpHeaderNames.TE)
  def trailer: Option[String] = get(HttpHeaderNames.TRAILER)
  def transferEncoding: Option[String] = get(HttpHeaderNames.TRANSFER_ENCODING)
  def upgrade: Option[String] = get(HttpHeaderNames.UPGRADE)
  def userAgent: Option[String] = get(HttpHeaderNames.USER_AGENT)
  def vary: Option[String] = get(HttpHeaderNames.VARY)
  def via: Option[String] = get(HttpHeaderNames.VIA)
  def warning: Option[String] = get(HttpHeaderNames.WARNING)
  def webSocketLocation: Option[String] = get(HttpHeaderNames.WEBSOCKET_LOCATION)
  def webSocketOrigin: Option[String] = get(HttpHeaderNames.WEBSOCKET_ORIGIN)
  def webSocketProtocol: Option[String] = get(HttpHeaderNames.WEBSOCKET_PROTOCOL)
  def wwwAuthenticate: Option[String] = get(HttpHeaderNames.WWW_AUTHENTICATE)
  
  //
  // Non-Standard
  //
  def dnt: Option[String] = get(NonStandardNames.DNT)
  def xSSL: Option[Boolean] = getBool(NonStandardNames.X_SSL)
  def xRequestedWith: Option[String] = get(NonStandardNames.X_REQUESTED_WITH)
  def xForwardedFor: Option[String] = get(NonStandardNames.X_FORWARDED_FOR)
  def xForwardedProto: Option[String] = get(NonStandardNames.X_FORWARDED_PROTO)
  def xFrameOptions: Option[String] = get(NonStandardNames.X_FRAME_OPTIONS)
  def xXSSProtection: Option[String] = get(NonStandardNames.X_XSS_PROTECTION)
  def xContentTypeOptions: Option[String] = get(NonStandardNames.X_CONTENT_TYPE_OPTIONS)
  def xPoweredBy: Option[String] = get(NonStandardNames.X_POWERED_BY)
  def xUACompatible: Option[String] = get(NonStandardNames.X_UA_COMPATIBLE)
  
  //
  // Basic Auth helpers
  //
  
  /**
   * The Basic Auth username and password from the Authorization header
   */
  def basicAuthUserAndPass: Option[(String,String)] = authorization.flatMap{ Headers.parseBasicAuthorization }
  
  /** The Basic Auth username from the Authorization header */
  def basicAuthUser: Option[String] = basicAuthUserAndPass.map{ _._1 }
  
  /** The Basic Auth password from the Authorization header */
  def basicAuthPass: Option[String] = basicAuthUserAndPass.map{ _._2 }
  
  //
  // Digest Auth Helpers
  //
  
  def digestAuthParams: Option[Map[String,String]] = digestAuthParamsWWWAuthenticate orElse digestAuthParamsAuthorization
  
  def digestAuthParamsWWWAuthenticate: Option[Map[String,String]] = wwwAuthenticate.flatMap{ Headers.parseDigestAuthParams }
  def digestAuthParamsAuthorization: Option[Map[String,String]] = authorization.flatMap{ Headers.parseDigestAuthParams }
  
  def digestAuthUser: Option[String] = digestAuthParams.flatMap{ _.get("username") }
  
  //
  // Misc Auth Helpers
  //
  
  /** Either the Basic or Digest auth username from the Authorization header */
  def authUser: Option[String] = basicAuthUser orElse digestAuthUser
}

final case class ImmutableHeaders(nettyHeaders: HttpHeaders) extends Headers {
  def withHeaders(headerValues: (CharSequence, Any)*): ImmutableHeaders = toMutableHeaders.withHeaders(headerValues:_*).toImmutableHeaders
  
  def withNoCache: ImmutableHeaders = toMutableHeaders.withNoCache.toImmutableHeaders
  
  def withCookies(c: Traversable[Cookie]): ImmutableHeaders = {
    val m = toMutableHeaders
    m.cookies = c
    m.toImmutableHeaders
  }
  
  def withSetCookies(c: Traversable[Cookie]): ImmutableHeaders = {
    val m = toMutableHeaders
    m.setCookies = c
    m.toImmutableHeaders
  }
}

final case class MutableHeaders(nettyHeaders: HttpHeaders = new DefaultHttpHeaders(false /* don't validate */)) extends Headers {
  import Headers._

  def add(name: CharSequence, value: String): Unit = add(name, Option(value))

  def add(name: CharSequence, value: Option[String]): Unit = value match {
    case Some(v) => nettyHeaders.add(name, v)
    case None    => // Don't add
  }

  def set(name: CharSequence, value: String): Unit = set(name, Option(value))

  def set(name: CharSequence, value: Option[String]): Unit = value match {
    case Some(v) => nettyHeaders.set(name, v)
    case None    => nettyHeaders.remove(name)
  }

  def setAll(name: CharSequence, values: Traversable[String]): Unit = {
    nettyHeaders.remove(name)
    values.foreach{ add(name, _) }
  }

  def setAll(name: CharSequence, values: Option[Traversable[String]]): Unit = setAll(name, values.getOrElse{ Nil })

  def setDate(name: CharSequence, value: Date): Unit = setDate(name, Option(value))
  def setDate(name: CharSequence, value: Option[Date]): Unit = set(name, value.map{ formatDate })

  def setImmutableDate(name: CharSequence, value: ImmutableDate): Unit = setImmutableDate(name, Option(value))
  def setImmutableDate(name: CharSequence, value: Option[ImmutableDate]): Unit = set(name, value.map{ formatDate })

  def setOffsetDateTime(name: CharSequence, value: OffsetDateTime): Unit = setOffsetDateTime(name, Option(value))
  def setOffsetDateTime(name: CharSequence, value: Option[OffsetDateTime]): Unit = set(name, value.map{ formatDate })

  def setZonedDateDate(name: CharSequence, value: ZonedDateTime): Unit = setZonedDateDate(name, Option(value))
  def setZonedDateDate(name: CharSequence, value: Option[ZonedDateTime]): Unit = set(name, value.map{ formatDate })

  def setInstant(name: CharSequence, value: Instant): Unit = setInstant(name, Option(value))
  def setInstant(name: CharSequence, value: Option[Instant]): Unit = set(name, value.map{ formatDate })

  def setLocalDateTime(name: CharSequence, value: LocalDateTime): Unit = setLocalDateTime(name, Option(value))
  def setLocalDateTime(name: CharSequence, value: Option[LocalDateTime]): Unit = set(name, value.map{ formatDate })

  def setInt(name: CharSequence, value: Int): Unit = set(name, value.toString)
  def setInt(name: CharSequence, value: Option[Int]): Unit = set(name, value.map{ _.toString })

  def setLong(name: CharSequence, value: Long): Unit = set(name, value.toString)
  def setLong(name: CharSequence, value: Option[Long]): Unit = set(name, value.map{ _.toString })

  def remove(name: String): Unit = nettyHeaders.remove(name)

  def withHeaders(headerValues: (CharSequence, Any)*): MutableHeaders = {
    headerValues.foreach { case (name, value) =>
      value match {
        case str: String => set(name, str)
        case date: Date => setDate(name, date)
        case date: ImmutableDate => setImmutableDate(name, date)
        case date: OffsetDateTime => setOffsetDateTime(name, date)
        case date: ZonedDateTime => setZonedDateDate(name, date)
        case date: Instant => setInstant(name, date)
        case date: LocalDateTime => setLocalDateTime(name, date)
        case int: Int => setInt(name, int)
        case long: Long => setLong(name, long)
        case _ => throw new IllegalArgumentException("Unknown Headers value type: "+value)
      }
    }

    this
  }

  def withNoCache: MutableHeaders = {
    // http://stackoverflow.com/a/2068407
    cacheControl = "no-cache, no-store, must-revalidate"
    pragma = "no-cache"
    expires = "0"
    this
  }

  def withCookies(c: Traversable[Cookie]): MutableHeaders = {
    cookies = c
    this
  }

  def withSetCookies(c: Traversable[Cookie]): MutableHeaders = {
    setCookies = c
    this
  }

  def accept_=(v: String): Unit = set(HttpHeaderNames.ACCEPT, v)
  def accept_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCEPT, v)

  def acceptCharset_=(v: String): Unit = set(HttpHeaderNames.ACCEPT_CHARSET, v)
  def acceptCharset_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCEPT_CHARSET, v)

  def acceptEncoding_=(v: String): Unit = set(HttpHeaderNames.ACCEPT_ENCODING, v)
  def acceptEncoding_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCEPT_ENCODING, v)

  def acceptLanguage_=(v: String): Unit = set(HttpHeaderNames.ACCEPT_LANGUAGE, v)
  def acceptLanguage_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCEPT_LANGUAGE, v)

  def acceptPatch_=(v: String): Unit = set(HttpHeaderNames.ACCEPT_PATCH, v)
  def acceptPatch_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCEPT_PATCH, v)

  def acceptRanges_=(v: String): Unit = set(HttpHeaderNames.ACCEPT_RANGES, v)
  def acceptRanges_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCEPT_RANGES, v)

  def accessControlAllowCredentials_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS, v)
  def accessControlAllowCredentials_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS, v)

  def accessControlAllowHeaders_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS, v)
  def accessControlAllowHeaders_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS, v)

  def accessControlAllowMethods_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS, v)
  def accessControlAllowMethods_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS, v)

  def accessControlAllowOrigin_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN, v)
  def accessControlAllowOrigin_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN, v)

  def accessControlExposeHeaders_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_EXPOSE_HEADERS, v)
  def accessControlExposeHeaders_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_EXPOSE_HEADERS, v)

  def accessControlMaxAge_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_MAX_AGE, v)
  def accessControlMaxAge_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_MAX_AGE, v)

  def accessControlRequestHeaders_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_REQUEST_HEADERS, v)
  def accessControlRequestHeaders_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_REQUEST_HEADERS, v)

  def accessControlRequestMethod_=(v: String): Unit = set(HttpHeaderNames.ACCESS_CONTROL_REQUEST_METHOD, v)
  def accessControlRequestMethod_=(v: Option[String]): Unit = set(HttpHeaderNames.ACCESS_CONTROL_REQUEST_METHOD, v)

  def age_=(v: Long): Unit = setLong(HttpHeaderNames.AGE, v)
  def age_=(v: Option[Long]): Unit = setLong(HttpHeaderNames.AGE, v)

  def allow_=(v: String): Unit = set(HttpHeaderNames.ALLOW, v)
  def allow_=(v: Option[String]): Unit = set(HttpHeaderNames.ALLOW, v)

  def authorization_=(v: String): Unit = set(HttpHeaderNames.AUTHORIZATION, v)
  def authorization_=(v: Option[String]): Unit = set(HttpHeaderNames.AUTHORIZATION, v)

  def cacheControl_=(v: String): Unit = set(HttpHeaderNames.CACHE_CONTROL, v)
  def cacheControl_=(v: Option[String]): Unit = set(HttpHeaderNames.CACHE_CONTROL, v)

  def connection_=(v: String): Unit = set(HttpHeaderNames.CONNECTION, v)
  def connection_=(v: Option[String]): Unit = set(HttpHeaderNames.CONNECTION, v)

  def contentBase_=(v: String): Unit = set(HttpHeaderNames.CONTENT_BASE, v)
  def contentBase_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_BASE, v)

  def contentDisposition_=(v: String): Unit = set(HttpHeaderNames.CONTENT_DISPOSITION, v)
  def contentDisposition_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_DISPOSITION, v)

  def contentDispositionAttachmentFileName_=(v: String): Unit = contentDisposition = makeContentDispositionAttachmentWithFileName(v)
  def contentDispositionAttachmentFileName_=(v: Option[String]): Unit = contentDisposition = v.map{ makeContentDispositionAttachmentWithFileName }

  def contentEncoding_=(v: String): Unit = set(HttpHeaderNames.CONTENT_ENCODING, v)
  def contentEncoding_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_ENCODING, v)

  def contentLanguage_=(v: String): Unit = set(HttpHeaderNames.CONTENT_LANGUAGE, v)
  def contentLanguage_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_LANGUAGE, v)

  def contentLength_=(v: Long): Unit = setLong(HttpHeaderNames.CONTENT_LENGTH, v)
  def contentLength_=(v: Option[Long]): Unit = setLong(HttpHeaderNames.CONTENT_LENGTH, v)

  def contentLocation_=(v: String): Unit = set(HttpHeaderNames.CONTENT_LOCATION, v)
  def contentLocation_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_LOCATION, v)

  def contentMD5_=(v: String): Unit = set(HttpHeaderNames.CONTENT_MD5, v)
  def contentMD5_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_MD5, v)

  def contentRange_=(v: String): Unit = set(HttpHeaderNames.CONTENT_RANGE, v)
  def contentRange_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_RANGE, v)

  def contentTransferEncoding_=(v: String): Unit = set(HttpHeaderNames.CONTENT_TRANSFER_ENCODING, v)
  def contentTransferEncoding_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_TRANSFER_ENCODING, v)

  def contentType_=(v: String): Unit = set(HttpHeaderNames.CONTENT_TYPE, v)
  def contentType_=(v: Option[String]): Unit = set(HttpHeaderNames.CONTENT_TYPE, v)

  def cookie_=(v: String): Unit = set(HttpHeaderNames.COOKIE, v)
  def cookie_=(v: Option[String]): Unit = set(HttpHeaderNames.COOKIE, v)

  def cookies_=(v: Traversable[Cookie]): Unit = cookie = if (v.nonEmpty) Some(ClientCookieEncoder.LAX.encode(v.toSeq.map{ _.toNettyCookie }.asJava)) else None
  def cookies_=(v: Option[Traversable[Cookie]]): Unit = cookies = v.getOrElse{ Nil }

  def date_=(v: ImmutableDate): Unit = setImmutableDate(HttpHeaderNames.DATE, v)
  def date_=(v: Option[ImmutableDate]): Unit = setImmutableDate(HttpHeaderNames.DATE, v)

  def eTag_=(v: String): Unit = set(HttpHeaderNames.ETAG, v)
  def eTag_=(v: Option[String]): Unit = set(HttpHeaderNames.ETAG, v)

  def expect_=(v: String): Unit = set(HttpHeaderNames.EXPECT, v)
  def expect_=(v: Option[String]): Unit = set(HttpHeaderNames.EXPECT, v)

  def expires_=(v: String): Unit = set(HttpHeaderNames.EXPIRES, v)
  def expires_=(v: ImmutableDate): Unit = setImmutableDate(HttpHeaderNames.EXPIRES, v)
  def expires_=(v: Option[ImmutableDate]): Unit = setImmutableDate(HttpHeaderNames.EXPIRES, v)

  def from_=(v: String): Unit = set(HttpHeaderNames.FROM, v)
  def from_=(v: Option[String]): Unit = set(HttpHeaderNames.FROM, v)

  def host_=(v: String): Unit = set(HttpHeaderNames.HOST, v)
  def host_=(v: Option[String]): Unit = set(HttpHeaderNames.HOST, v)

  def ifMatch_=(v: String): Unit = set(HttpHeaderNames.IF_MATCH, v)
  def ifMatch_=(v: Option[String]): Unit = set(HttpHeaderNames.IF_MATCH, v)

  def ifModifiedSince_=(v: ImmutableDate): Unit = setImmutableDate(HttpHeaderNames.IF_MODIFIED_SINCE, v)
  def ifModifiedSince_=(v: Option[ImmutableDate]): Unit = setImmutableDate(HttpHeaderNames.IF_MODIFIED_SINCE, v)

  def ifNoneMatch_=(v: String): Unit = set(HttpHeaderNames.IF_NONE_MATCH, v)
  def ifNoneMatch_=(v: Option[String]): Unit = set(HttpHeaderNames.IF_NONE_MATCH, v)

  def ifRange_=(v: String): Unit = set(HttpHeaderNames.IF_RANGE, v)
  def ifRange_=(v: Option[String]): Unit = set(HttpHeaderNames.IF_RANGE, v)

  def ifUnmodifiedSince_=(v: ImmutableDate): Unit = setImmutableDate(HttpHeaderNames.IF_UNMODIFIED_SINCE, v)
  def ifUnmodifiedSince_=(v: Option[ImmutableDate]): Unit = setImmutableDate(HttpHeaderNames.IF_UNMODIFIED_SINCE, v)

  def lastModified_=(v: ImmutableDate): Unit = setImmutableDate(HttpHeaderNames.LAST_MODIFIED, v)
  def lastModified_=(v: Option[ImmutableDate]): Unit = setImmutableDate(HttpHeaderNames.LAST_MODIFIED, v)

  def location_=(v: String): Unit = set(HttpHeaderNames.LOCATION, v)
  def location_=(v: Option[String]): Unit = set(HttpHeaderNames.LOCATION, v)

  def maxForwards_=(v: String): Unit = set(HttpHeaderNames.MAX_FORWARDS, v)
  def maxForwards_=(v: Option[String]): Unit = set(HttpHeaderNames.MAX_FORWARDS, v)

  def origin_=(v: String): Unit = set(HttpHeaderNames.ORIGIN, v)
  def origin_=(v: Option[String]): Unit = set(HttpHeaderNames.ORIGIN, v)

  def pragma_=(v: String): Unit = set(HttpHeaderNames.PRAGMA, v)
  def pragma_=(v: Option[String]): Unit = set(HttpHeaderNames.PRAGMA, v)

  def proxyAuthencate_=(v: String): Unit = set(HttpHeaderNames.PROXY_AUTHENTICATE, v)
  def proxyAuthencate_=(v: Option[String]): Unit = set(HttpHeaderNames.PROXY_AUTHENTICATE, v)

  def proxyAuthorization_=(v: String): Unit = set(HttpHeaderNames.PROXY_AUTHORIZATION, v)
  def proxyAuthorization_=(v: Option[String]): Unit = set(HttpHeaderNames.PROXY_AUTHORIZATION, v)

  def range_=(v: String): Unit = set(HttpHeaderNames.RANGE, v)
  def range_=(v: Option[String]): Unit = set(HttpHeaderNames.RANGE, v)

  def referer_=(v: String): Unit = set(HttpHeaderNames.REFERER, v)
  def referer_=(v: Option[String]): Unit = set(HttpHeaderNames.REFERER, v)

  def retryAfter_=(v: String): Unit = set(HttpHeaderNames.RETRY_AFTER, v)
  def retryAfter_=(v: Option[String]): Unit = set(HttpHeaderNames.RETRY_AFTER, v)

  def secWebSocketAccept_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_ACCEPT, v)
  def secWebSocketAccept_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_ACCEPT, v)

  def secWebSocketKey_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_KEY, v)
  def secWebSocketKey_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_KEY, v)

  def secWebSocketKey1_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_KEY1, v)
  def secWebSocketKey1_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_KEY1, v)

  def secWebSocketKey2_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_KEY2, v)
  def secWebSocketKey2_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_KEY2, v)

  def secWebSocketLocation_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_LOCATION, v)
  def secWebSocketLocation_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_LOCATION, v)

  def secWebSocketOrigin_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_ORIGIN, v)
  def secWebSocketOrigin_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_ORIGIN, v)

  def secWebSocketProtocol_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_PROTOCOL, v)
  def secWebSocketProtocol_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_PROTOCOL, v)

  def secWebSocketVersion_=(v: String): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_VERSION, v)
  def secWebSocketVersion_=(v: Option[String]): Unit = set(HttpHeaderNames.SEC_WEBSOCKET_VERSION, v)

  def server_=(v: String): Unit = set(HttpHeaderNames.SERVER, v)
  def server_=(v: Option[String]): Unit = set(HttpHeaderNames.SERVER, v)

  def setCookie_=(v: Traversable[String]): Unit = setAll(HttpHeaderNames.SET_COOKIE, v)
  def setCookie_=(v: Option[Traversable[String]]): Unit = setAll(HttpHeaderNames.SET_COOKIE, v)

  def setCookies_=(v: Traversable[Cookie]): Unit = setCookie = if (v.nonEmpty) Some(v.toSeq.map{ c: Cookie => ServerCookieEncoder.LAX.encode(c.toNettyCookie) }) else None
  def setCookies_=(v: Option[Traversable[Cookie]]): Unit = setCookies = v.getOrElse{ Nil }

  def setCookie2_=(v: Traversable[String]): Unit = setAll(HttpHeaderNames.SET_COOKIE2, v)
  def setCookie2_=(v: Option[Traversable[String]]): Unit = setAll(HttpHeaderNames.SET_COOKIE2, v)

  def te_=(v: String): Unit = set(HttpHeaderNames.TE, v)
  def te_=(v: Option[String]): Unit = set(HttpHeaderNames.TE, v)

  def trailer_=(v: String): Unit = set(HttpHeaderNames.TRAILER, v)
  def trailer_=(v: Option[String]): Unit = set(HttpHeaderNames.TRAILER, v)

  def transferEncoding_=(v: String): Unit = set(HttpHeaderNames.TRANSFER_ENCODING, v)
  def transferEncoding_=(v: Option[String]): Unit = set(HttpHeaderNames.TRANSFER_ENCODING, v)

  def upgrade_=(v: String): Unit = set(HttpHeaderNames.UPGRADE, v)
  def upgrade_=(v: Option[String]): Unit = set(HttpHeaderNames.UPGRADE, v)

  def userAgent_=(v: String): Unit = set(HttpHeaderNames.USER_AGENT, v)
  def userAgent_=(v: Option[String]): Unit = set(HttpHeaderNames.USER_AGENT, v)

  def vary_=(v: String): Unit = set(HttpHeaderNames.VARY, v)
  def vary_=(v: Option[String]): Unit = set(HttpHeaderNames.VARY, v)

  def via_=(v: String): Unit = set(HttpHeaderNames.VIA, v)
  def via_=(v: Option[String]): Unit = set(HttpHeaderNames.VIA, v)

  def warning_=(v: String): Unit = set(HttpHeaderNames.WARNING, v)
  def warning_=(v: Option[String]): Unit = set(HttpHeaderNames.WARNING, v)

  def webSocketLocation_=(v: String): Unit = set(HttpHeaderNames.WEBSOCKET_LOCATION, v)
  def webSocketLocation_=(v: Option[String]): Unit = set(HttpHeaderNames.WEBSOCKET_LOCATION, v)

  def webSocketOrigin_=(v: String): Unit = set(HttpHeaderNames.WEBSOCKET_ORIGIN, v)
  def webSocketOrigin_=(v: Option[String]): Unit = set(HttpHeaderNames.WEBSOCKET_ORIGIN, v)

  def webSocketProtocol_=(v: String): Unit = set(HttpHeaderNames.WEBSOCKET_PROTOCOL, v)
  def webSocketProtocol_=(v: Option[String]): Unit = set(HttpHeaderNames.WEBSOCKET_PROTOCOL, v)

  def wwwAuthenticate_=(v: String): Unit = set(HttpHeaderNames.WWW_AUTHENTICATE, v)
  def wwwAuthenticate_=(v: Option[String]): Unit = set(HttpHeaderNames.WWW_AUTHENTICATE, v)


  //
  // Non-Standard
  //
  def dnt_=(v: String): Unit = set(NonStandardNames.DNT, v)
  def dnt_=(v: Option[String]): Unit = set(NonStandardNames.DNT, v)

  def xRequestedWith_=(v: String): Unit = set(NonStandardNames.X_REQUESTED_WITH, v)
  def xRequestedWith_=(v: Option[String]): Unit = set(NonStandardNames.X_REQUESTED_WITH, v)

  def xForwardedFor_=(v: String): Unit = set(NonStandardNames.X_FORWARDED_FOR, v)
  def xForwardedFor_=(v: Option[String]): Unit = set(NonStandardNames.X_FORWARDED_FOR, v)

  def xForwardedProto_=(v: String): Unit = set(NonStandardNames.X_FORWARDED_PROTO, v)
  def xForwardedProto_=(v: Option[String]): Unit = set(NonStandardNames.X_FORWARDED_PROTO, v)

  def xFrameOptions_=(v: String): Unit = set(NonStandardNames.X_FRAME_OPTIONS, v)
  def xFrameOptions_=(v: Option[String]): Unit = set(NonStandardNames.X_FRAME_OPTIONS, v)

  def xXSSProtection_=(v: String): Unit = set(NonStandardNames.X_XSS_PROTECTION, v)
  def xXSSProtection_=(v: Option[String]): Unit = set(NonStandardNames.X_XSS_PROTECTION, v)

  def xContentTypeOptions_=(v: String): Unit = set(NonStandardNames.X_CONTENT_TYPE_OPTIONS, v)
  def xContentTypeOptions_=(v: Option[String]): Unit = set(NonStandardNames.X_CONTENT_TYPE_OPTIONS, v)

  def xPoweredBy_=(v: String): Unit = set(NonStandardNames.X_POWERED_BY, v)
  def xPoweredBy_=(v: Option[String]): Unit = set(NonStandardNames.X_POWERED_BY, v)

  def xUACompatible_=(v: String): Unit = set(NonStandardNames.X_UA_COMPATIBLE, v)
  def xUACompatible_=(v: Option[String]): Unit = set(NonStandardNames.X_UA_COMPATIBLE, v)

  //
  // Private Helpers
  //

  private def makeContentDispositionAttachmentWithFileName(v: String): String = {
    val escapedName: String = v.replace("\"", "\\\"") // Replace " with \"
    s"""attachment; filename="$escapedName""""
  }
}