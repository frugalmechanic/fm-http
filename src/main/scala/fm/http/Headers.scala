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
import io.netty.handler.codec.http.{DefaultHttpHeaders, HttpHeaders}
import io.netty.handler.codec.http.cookie.{ClientCookieEncoder, ServerCookieEncoder}
import java.time._
import java.time.format.DateTimeFormatter
import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.matching.Regex

object Headers {  
  val empty: Headers = ImmutableHeaders(HttpHeaders.EMPTY_HEADERS)
  
  def apply(headerValues: (String, Any)*): ImmutableHeaders = {
    val headers: MutableHeaders = MutableHeaders()
    headers.withHeaders(headerValues:_*)
    
    // Convert to immutable headers - this is safe since we know we won't be touching the MutableHeaders again
    ImmutableHeaders(headers.nettyHeaders)
  }
  
  def noCache(headerValues: (String, Any)*): ImmutableHeaders = {
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
//    if (s.isBlank) return None
//    val cleaned: String = if(s.contains(";")) s.substring(0, s.indexOf(";")) else s
//    Try{ ZonedDateTime.parse(cleaned, DateTimeFormatter.RFC_1123_DATE_TIME) }.toOption
//  }

  private[http] def parseImmutableDate(s: String): Option[ImmutableDate] = {
    if (s.isBlank) return None
    val cleaned: String = if(s.contains(";")) s.substring(0, s.indexOf(";")) else s
    Try{ ZonedDateTime.parse(cleaned, DateTimeFormatter.RFC_1123_DATE_TIME) }.toOption.map{ _.toInstant }.map{ ImmutableDate(_) }
  }

  // A very very simple Regex for parsing the filename from the Content-Disposition Header
  private[http] val ContentDispositionAttachmentFileNamePattern: Regex = """attachment; filename="(.*)"""".r

  private[http] def parseContentDispositionAttachmentFileName(s: String): Option[String] = s match {
    case ContentDispositionAttachmentFileNamePattern(name) => name.toBlankOption
    case _ => None
  }

  private[http] object AdditionalNames {
    //
    // Standard but not part of Netty's Names
    //
    val CONTENT_DISPOSITION = "Content-Disposition"
  }

  private[http] object NonStandardNames {
    //
    // Non-Standard Request Headers
    //
    val DNT: String = "DNT"
    val X_REQUESTED_WITH: String = "X-Requested-With"
    val X_FORWARDED_FOR: String = "X-Forwarded-For"
    val X_FORWARDED_PROTO: String = "X-Forwarded-Proto"
    val X_SSL: String = "X-SSL"
   
    //
    // Non-Standard Response Headers
    //
    val X_FRAME_OPTIONS: String = "X-Frame-Options"
    val X_XSS_PROTECTION: String = "X-XSS-Protection"
    val X_CONTENT_TYPE_OPTIONS: String = "X-Content-Type-Options"
    val X_POWERED_BY: String = "X-Powered-By"
    val X_UA_COMPATIBLE: String = "X-UA-Compatible"
  }
  
  private val BasicAuthHeader: Regex = """Basic (.+)""".r
  private val BasicAuthSplit: Regex = """(.+):(.+)""".r
  
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
  import HttpHeaders.Names
  
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
  def withHeaders(headerValues: (String, Any)*): Headers
  
  /** Add the Cache-Control and Expires headers to prevent caching of this response */
  def withNoCache: Headers
  
  /** These are the client side cookies that are being sent with the request */
  def withCookies(c: Traversable[Cookie]): Headers
  
  /** These are the server side cookies that are being sent with the response */
  def withSetCookies(c: Traversable[Cookie]): Headers
  
  /** These are the client side cookies that are being sent with the request */
  def addCookie(c: Cookie): Headers = {
    withSetCookies(cookies.filterNot{ _.name === c.name } :+ c)
  }
  
  /** These are the server side cookies that are being sent with the response */
  def addSetCookie(c: Cookie): Headers = {
    withSetCookies(setCookies.filterNot{ _.name === c.name } :+ c)
  }
  
  override def toString: String = self.map{ case (k,v) => s"$k: $v" }.mkString("\n")
  
  def apply(name: String): String = get(name).getOrElse{ throw new NoSuchElementException("No header value for: "+name) }
  def get(name: String): Option[String] = Option(nettyHeaders.get(name))
  
  def getAll(name: String): Vector[String] = nettyHeaders.getAll(name).asScala.toVector
  
  def getBool(name: String): Option[Boolean] = get(name).flatMap{ _.parseBoolean }
  def getDate(name: String): Option[ImmutableDate] = get(name).flatMap{ parseImmutableDate }
  def getInt(name: String): Option[Int] = get(name).flatMap{ _.toIntOption }
  def getLong(name: String): Option[Long] = get(name).flatMap{ _.toLongOption }
  
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
  
  def accept: Option[String] = get(Names.ACCEPT)
  def acceptCharset: Option[String] = get(Names.ACCEPT_CHARSET)
  def acceptEncoding: Option[String] = get(Names.ACCEPT_ENCODING)
  def acceptLanguage: Option[String] = get(Names.ACCEPT_LANGUAGE)
  def acceptPatch: Option[String] = get(Names.ACCEPT_PATCH)
  def acceptRanges: Option[String] = get(Names.ACCEPT_RANGES)
  def accessControlAllowCredentials: Option[String] = get(Names.ACCESS_CONTROL_ALLOW_CREDENTIALS)
  def accessControlAllowHeaders: Option[String] = get(Names.ACCESS_CONTROL_ALLOW_HEADERS)
  def accessControlAllowMethods: Option[String] = get(Names.ACCESS_CONTROL_ALLOW_METHODS)
  def accessControlAllowOrigin: Option[String] = get(Names.ACCESS_CONTROL_ALLOW_ORIGIN)
  def accessControlExposeHeaders: Option[String] = get(Names.ACCESS_CONTROL_EXPOSE_HEADERS)
  def accessControlMaxAge: Option[String] = get(Names.ACCESS_CONTROL_MAX_AGE)
  def accessControlRequestHEaders: Option[String] = get(Names.ACCESS_CONTROL_REQUEST_HEADERS)
  def accessControlRequestMethod: Option[String] = get(Names.ACCESS_CONTROL_REQUEST_METHOD)
  def age: Option[Long] = getLong(Names.AGE)
  def allow: Option[String] = get(Names.ALLOW)
  def authorization: Option[String] = get(Names.AUTHORIZATION)
  def cacheControl: Option[String] = get(Names.CACHE_CONTROL)
  def connection: Option[String] = get(Names.CONNECTION)
  def contentBase: Option[String] = get(Names.CONTENT_BASE)
  def contentDisposition: Option[String] = get(AdditionalNames.CONTENT_DISPOSITION)
  def contentDispositionAttachmentFileName: Option[String] = contentDisposition.flatMap{ parseContentDispositionAttachmentFileName }
  def contentEncoding: Option[String] = get(Names.CONTENT_ENCODING)
  def contentLanguage: Option[String] = get(Names.CONTENT_LANGUAGE)
  def contentLength: Option[Long] = getLong(Names.CONTENT_LENGTH)
  def contentLocation: Option[String] = get(Names.CONTENT_LOCATION)
  def contentMD5: Option[String] = get(Names.CONTENT_MD5)
  def contentRange: Option[String] = get(Names.CONTENT_RANGE)
  def contentTransferEncoding: Option[String] = get(Names.CONTENT_TRANSFER_ENCODING)
  def contentType: Option[String] = get(Names.CONTENT_TYPE)
  
  /** This is the raw value of the client side cookies that are being sent with the request */
  def cookie: Option[String] = get(Names.COOKIE)
  
  /** These are the client side cookies that are being sent with the request */
  def cookies: Vector[Cookie] = cookie.map{ Cookie.tryParseCookieHeader }.getOrElse{ Vector.empty }
  
  def date: Option[ImmutableDate] = getDate(Names.DATE)
  def eTag: Option[String] = get(Names.ETAG)
  def expect: Option[String] = get(Names.EXPECT)
  def expires: Option[ImmutableDate] = getDate(Names.EXPIRES)
  def from: Option[String] = get(Names.FROM)
  def host: Option[String] = get(Names.HOST)
  def ifMatch: Option[String] = get(Names.IF_MATCH)
  def ifModifiedSince: Option[ImmutableDate] = getDate(Names.IF_MODIFIED_SINCE)
  def ifNoneMatch: Option[String] = get(Names.IF_NONE_MATCH)
  def ifRange: Option[String] = get(Names.IF_RANGE)
  def ifUnmodifiedSince: Option[ImmutableDate] = getDate(Names.IF_UNMODIFIED_SINCE)
  def lastModified: Option[ImmutableDate] = getDate(Names.LAST_MODIFIED)
  def location: Option[String] = get(Names.LOCATION)
  def maxForwards: Option[String] = get(Names.MAX_FORWARDS)
  def origin: Option[String] = get(Names.ORIGIN)
  def pragma: Option[String] = get(Names.PRAGMA)
  def proxyAuthencate: Option[String] = get(Names.PROXY_AUTHENTICATE)
  def proxyAuthorization: Option[String] = get(Names.PROXY_AUTHORIZATION)
  def range: Option[String] = get(Names.RANGE)
  def referer: Option[String] = get(Names.REFERER)
  def retryAfter: Option[String] = get(Names.RETRY_AFTER)
  def secWebSocketAccept: Option[String] = get(Names.SEC_WEBSOCKET_ACCEPT)
  def secWebSocketKey: Option[String] = get(Names.SEC_WEBSOCKET_KEY)
  def secWebSocketKey1: Option[String] = get(Names.SEC_WEBSOCKET_KEY1)
  def secWebSocketKey2: Option[String] = get(Names.SEC_WEBSOCKET_KEY2)
  def secWebSocketLocation: Option[String] = get(Names.SEC_WEBSOCKET_LOCATION)
  def secWebSocketOrigin: Option[String] = get(Names.SEC_WEBSOCKET_ORIGIN)
  def secWebSocketProtocol: Option[String] = get(Names.SEC_WEBSOCKET_PROTOCOL)
  def secWebSocketVersion: Option[String] = get(Names.SEC_WEBSOCKET_VERSION)
  def server: Option[String] = get(Names.SERVER)
  
  /** This is the raw value of the server side cookies that are being sent with the response */
  def setCookie: Vector[String] = getAll(Names.SET_COOKIE)
  
  /** These are the server side cookies that are being sent with the response */
  def setCookies: Vector[Cookie] = setCookie.flatMap{ Cookie.tryParseSetCookieHeader }

  def setCookie2: Vector[String] = getAll(Names.SET_COOKIE2)
  def te: Option[String] = get(Names.TE)
  def trailer: Option[String] = get(Names.TRAILER)
  def transferEncoding: Option[String] = get(Names.TRANSFER_ENCODING)
  def upgrade: Option[String] = get(Names.UPGRADE)
  def userAgent: Option[String] = get(Names.USER_AGENT)
  def vary: Option[String] = get(Names.VARY)
  def via: Option[String] = get(Names.VIA)
  def warning: Option[String] = get(Names.WARNING)
  def webSocketLocation: Option[String] = get(Names.WEBSOCKET_LOCATION)
  def webSocketOrigin: Option[String] = get(Names.WEBSOCKET_ORIGIN)
  def webSocketProtocol: Option[String] = get(Names.WEBSOCKET_PROTOCOL)
  def wwwAuthenticate: Option[String] = get(Names.WWW_AUTHENTICATE)
  
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
  def withHeaders(headerValues: (String, Any)*): ImmutableHeaders = toMutableHeaders.withHeaders(headerValues:_*).toImmutableHeaders
  
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
  import HttpHeaders.Names

  def add(name: String, value: String): Unit = add(name, Option(value))

  def add(name: String, value: Option[String]): Unit = value match {
    case Some(v) => nettyHeaders.add(name, v)
    case None    => // Don't add
  }

  def set(name: String, value: String): Unit = set(name, Option(value))

  def set(name: String, value: Option[String]): Unit = value match {
    case Some(v) => nettyHeaders.set(name, v)
    case None    => nettyHeaders.remove(name)
  }

  def setAll(name: String, values: Traversable[String]): Unit = {
    nettyHeaders.remove(name)
    values.foreach{ add(name, _) }
  }

  def setAll(name: String, values: Option[Traversable[String]]): Unit = setAll(name, values.getOrElse{ Nil })

  def setDate(name: String, value: Date): Unit = setDate(name, Option(value))
  def setDate(name: String, value: Option[Date]): Unit = set(name, value.map{ formatDate })

  def setImmutableDate(name: String, value: ImmutableDate): Unit = setImmutableDate(name, Option(value))
  def setImmutableDate(name: String, value: Option[ImmutableDate]): Unit = set(name, value.map{ formatDate })

  def setOffsetDateTime(name: String, value: OffsetDateTime): Unit = setOffsetDateTime(name, Option(value))
  def setOffsetDateTime(name: String, value: Option[OffsetDateTime]): Unit = set(name, value.map{ formatDate })

  def setZonedDateDate(name: String, value: ZonedDateTime): Unit = setZonedDateDate(name, Option(value))
  def setZonedDateDate(name: String, value: Option[ZonedDateTime]): Unit = set(name, value.map{ formatDate })

  def setInstant(name: String, value: Instant): Unit = setInstant(name, Option(value))
  def setInstant(name: String, value: Option[Instant]): Unit = set(name, value.map{ formatDate })

  def setLocalDateTime(name: String, value: LocalDateTime): Unit = setLocalDateTime(name, Option(value))
  def setLocalDateTime(name: String, value: Option[LocalDateTime]): Unit = set(name, value.map{ formatDate })

  def setInt(name: String, value: Int): Unit = set(name, value.toString)
  def setInt(name: String, value: Option[Int]): Unit = set(name, value.map{ _.toString })

  def setLong(name: String, value: Long): Unit = set(name, value.toString)
  def setLong(name: String, value: Option[Long]): Unit = set(name, value.map{ _.toString })

  def withHeaders(headerValues: (String, Any)*): MutableHeaders = {
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

  def accept_=(v: String): Unit = set(Names.ACCEPT, v)
  def accept_=(v: Option[String]): Unit = set(Names.ACCEPT, v)

  def acceptCharset_=(v: String): Unit = set(Names.ACCEPT_CHARSET, v)
  def acceptCharset_=(v: Option[String]): Unit = set(Names.ACCEPT_CHARSET, v)

  def acceptEncoding_=(v: String): Unit = set(Names.ACCEPT_ENCODING, v)
  def acceptEncoding_=(v: Option[String]): Unit = set(Names.ACCEPT_ENCODING, v)

  def acceptLanguage_=(v: String): Unit = set(Names.ACCEPT_LANGUAGE, v)
  def acceptLanguage_=(v: Option[String]): Unit = set(Names.ACCEPT_LANGUAGE, v)

  def acceptPatch_=(v: String): Unit = set(Names.ACCEPT_PATCH, v)
  def acceptPatch_=(v: Option[String]): Unit = set(Names.ACCEPT_PATCH, v)

  def acceptRanges_=(v: String): Unit = set(Names.ACCEPT_RANGES, v)
  def acceptRanges_=(v: Option[String]): Unit = set(Names.ACCEPT_RANGES, v)

  def accessControlAllowCredentials_=(v: String): Unit = set(Names.ACCESS_CONTROL_ALLOW_CREDENTIALS, v)
  def accessControlAllowCredentials_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_ALLOW_CREDENTIALS, v)

  def accessControlAllowHeaders_=(v: String): Unit = set(Names.ACCESS_CONTROL_ALLOW_HEADERS, v)
  def accessControlAllowHeaders_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_ALLOW_HEADERS, v)

  def accessControlAllowMethods_=(v: String): Unit = set(Names.ACCESS_CONTROL_ALLOW_METHODS, v)
  def accessControlAllowMethods_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_ALLOW_METHODS, v)

  def accessControlAllowOrigin_=(v: String): Unit = set(Names.ACCESS_CONTROL_ALLOW_ORIGIN, v)
  def accessControlAllowOrigin_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_ALLOW_ORIGIN, v)

  def accessControlExposeHeaders_=(v: String): Unit = set(Names.ACCESS_CONTROL_EXPOSE_HEADERS, v)
  def accessControlExposeHeaders_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_EXPOSE_HEADERS, v)

  def accessControlMaxAge_=(v: String): Unit = set(Names.ACCESS_CONTROL_MAX_AGE, v)
  def accessControlMaxAge_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_MAX_AGE, v)

  def accessControlRequestHEaders_=(v: String): Unit = set(Names.ACCESS_CONTROL_REQUEST_HEADERS, v)
  def accessControlRequestHEaders_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_REQUEST_HEADERS, v)

  def accessControlRequestMethod_=(v: String): Unit = set(Names.ACCESS_CONTROL_REQUEST_METHOD, v)
  def accessControlRequestMethod_=(v: Option[String]): Unit = set(Names.ACCESS_CONTROL_REQUEST_METHOD, v)

  def age_=(v: Long): Unit = setLong(Names.AGE, v)
  def age_=(v: Option[Long]): Unit = setLong(Names.AGE, v)

  def allow_=(v: String): Unit = set(Names.ALLOW, v)
  def allow_=(v: Option[String]): Unit = set(Names.ALLOW, v)

  def authorization_=(v: String): Unit = set(Names.AUTHORIZATION, v)
  def authorization_=(v: Option[String]): Unit = set(Names.AUTHORIZATION, v)

  def cacheControl_=(v: String): Unit = set(Names.CACHE_CONTROL, v)
  def cacheControl_=(v: Option[String]): Unit = set(Names.CACHE_CONTROL, v)

  def connection_=(v: String): Unit = set(Names.CONNECTION, v)
  def connection_=(v: Option[String]): Unit = set(Names.CONNECTION, v)

  def contentBase_=(v: String): Unit = set(Names.CONTENT_BASE, v)
  def contentBase_=(v: Option[String]): Unit = set(Names.CONTENT_BASE, v)

  def contentDisposition_=(v: String): Unit = set(AdditionalNames.CONTENT_DISPOSITION, v)
  def contentDisposition_=(v: Option[String]): Unit = set(AdditionalNames.CONTENT_DISPOSITION, v)

  def contentDispositionAttachmentFileName_=(v: String): Unit = contentDisposition = makeContentDispositionAttachmentWithFileName(v)
  def contentDispositionAttachmentFileName_=(v: Option[String]): Unit = contentDisposition = v.map{ makeContentDispositionAttachmentWithFileName }

  def contentEncoding_=(v: String): Unit = set(Names.CONTENT_ENCODING, v)
  def contentEncoding_=(v: Option[String]): Unit = set(Names.CONTENT_ENCODING, v)

  def contentLanguage_=(v: String): Unit = set(Names.CONTENT_LANGUAGE, v)
  def contentLanguage_=(v: Option[String]): Unit = set(Names.CONTENT_LANGUAGE, v)

  def contentLength_=(v: Long): Unit = setLong(Names.CONTENT_LENGTH, v)
  def contentLength_=(v: Option[Long]): Unit = setLong(Names.CONTENT_LENGTH, v)

  def contentLocation_=(v: String): Unit = set(Names.CONTENT_LOCATION, v)
  def contentLocation_=(v: Option[String]): Unit = set(Names.CONTENT_LOCATION, v)

  def contentMD5_=(v: String): Unit = set(Names.CONTENT_MD5, v)
  def contentMD5_=(v: Option[String]): Unit = set(Names.CONTENT_MD5, v)

  def contentRange_=(v: String): Unit = set(Names.CONTENT_RANGE, v)
  def contentRange_=(v: Option[String]): Unit = set(Names.CONTENT_RANGE, v)

  def contentTransferEncoding_=(v: String): Unit = set(Names.CONTENT_TRANSFER_ENCODING, v)
  def contentTransferEncoding_=(v: Option[String]): Unit = set(Names.CONTENT_TRANSFER_ENCODING, v)

  def contentType_=(v: String): Unit = set(Names.CONTENT_TYPE, v)
  def contentType_=(v: Option[String]): Unit = set(Names.CONTENT_TYPE, v)

  def cookie_=(v: String): Unit = set(Names.COOKIE, v)
  def cookie_=(v: Option[String]): Unit = set(Names.COOKIE, v)

  def cookies_=(v: Traversable[Cookie]): Unit = cookie = if (v.nonEmpty) Some(ClientCookieEncoder.LAX.encode(v.toSeq.map{ _.toNettyCookie }.asJava)) else None
  def cookies_=(v: Option[Traversable[Cookie]]): Unit = cookies = v.getOrElse{ Nil }

  def date_=(v: ImmutableDate): Unit = setImmutableDate(Names.DATE, v)
  def date_=(v: Option[ImmutableDate]): Unit = setImmutableDate(Names.DATE, v)

  def eTag_=(v: String): Unit = set(Names.ETAG, v)
  def eTag_=(v: Option[String]): Unit = set(Names.ETAG, v)

  def expect_=(v: String): Unit = set(Names.EXPECT, v)
  def expect_=(v: Option[String]): Unit = set(Names.EXPECT, v)

  def expires_=(v: String): Unit = set(Names.EXPIRES, v)
  def expires_=(v: ImmutableDate): Unit = setImmutableDate(Names.EXPIRES, v)
  def expires_=(v: Option[ImmutableDate]): Unit = setImmutableDate(Names.EXPIRES, v)

  def from_=(v: String): Unit = set(Names.FROM, v)
  def from_=(v: Option[String]): Unit = set(Names.FROM, v)

  def host_=(v: String): Unit = set(Names.HOST, v)
  def host_=(v: Option[String]): Unit = set(Names.HOST, v)

  def ifMatch_=(v: String): Unit = set(Names.IF_MATCH, v)
  def ifMatch_=(v: Option[String]): Unit = set(Names.IF_MATCH, v)

  def ifModifiedSince_=(v: ImmutableDate): Unit = setImmutableDate(Names.IF_MODIFIED_SINCE, v)
  def ifModifiedSince_=(v: Option[ImmutableDate]): Unit = setImmutableDate(Names.IF_MODIFIED_SINCE, v)

  def ifNoneMatch_=(v: String): Unit = set(Names.IF_NONE_MATCH, v)
  def ifNoneMatch_=(v: Option[String]): Unit = set(Names.IF_NONE_MATCH, v)

  def ifRange_=(v: String): Unit = set(Names.IF_RANGE, v)
  def ifRange_=(v: Option[String]): Unit = set(Names.IF_RANGE, v)

  def ifUnmodifiedSince_=(v: ImmutableDate): Unit = setImmutableDate(Names.IF_UNMODIFIED_SINCE, v)
  def ifUnmodifiedSince_=(v: Option[ImmutableDate]): Unit = setImmutableDate(Names.IF_UNMODIFIED_SINCE, v)

  def lastModified_=(v: ImmutableDate): Unit = setImmutableDate(Names.LAST_MODIFIED, v)
  def lastModified_=(v: Option[ImmutableDate]): Unit = setImmutableDate(Names.LAST_MODIFIED, v)

  def location_=(v: String): Unit = set(Names.LOCATION, v)
  def location_=(v: Option[String]): Unit = set(Names.LOCATION, v)

  def maxForwards_=(v: String): Unit = set(Names.MAX_FORWARDS, v)
  def maxForwards_=(v: Option[String]): Unit = set(Names.MAX_FORWARDS, v)

  def origin_=(v: String): Unit = set(Names.ORIGIN, v)
  def origin_=(v: Option[String]): Unit = set(Names.ORIGIN, v)

  def pragma_=(v: String): Unit = set(Names.PRAGMA, v)
  def pragma_=(v: Option[String]): Unit = set(Names.PRAGMA, v)

  def proxyAuthencate_=(v: String): Unit = set(Names.PROXY_AUTHENTICATE, v)
  def proxyAuthencate_=(v: Option[String]): Unit = set(Names.PROXY_AUTHENTICATE, v)

  def proxyAuthorization_=(v: String): Unit = set(Names.PROXY_AUTHORIZATION, v)
  def proxyAuthorization_=(v: Option[String]): Unit = set(Names.PROXY_AUTHORIZATION, v)

  def range_=(v: String): Unit = set(Names.RANGE, v)
  def range_=(v: Option[String]): Unit = set(Names.RANGE, v)

  def referer_=(v: String): Unit = set(Names.REFERER, v)
  def referer_=(v: Option[String]): Unit = set(Names.REFERER, v)

  def retryAfter_=(v: String): Unit = set(Names.RETRY_AFTER, v)
  def retryAfter_=(v: Option[String]): Unit = set(Names.RETRY_AFTER, v)

  def secWebSocketAccept_=(v: String): Unit = set(Names.SEC_WEBSOCKET_ACCEPT, v)
  def secWebSocketAccept_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_ACCEPT, v)

  def secWebSocketKey_=(v: String): Unit = set(Names.SEC_WEBSOCKET_KEY, v)
  def secWebSocketKey_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_KEY, v)

  def secWebSocketKey1_=(v: String): Unit = set(Names.SEC_WEBSOCKET_KEY1, v)
  def secWebSocketKey1_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_KEY1, v)

  def secWebSocketKey2_=(v: String): Unit = set(Names.SEC_WEBSOCKET_KEY2, v)
  def secWebSocketKey2_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_KEY2, v)

  def secWebSocketLocation_=(v: String): Unit = set(Names.SEC_WEBSOCKET_LOCATION, v)
  def secWebSocketLocation_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_LOCATION, v)

  def secWebSocketOrigin_=(v: String): Unit = set(Names.SEC_WEBSOCKET_ORIGIN, v)
  def secWebSocketOrigin_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_ORIGIN, v)

  def secWebSocketProtocol_=(v: String): Unit = set(Names.SEC_WEBSOCKET_PROTOCOL, v)
  def secWebSocketProtocol_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_PROTOCOL, v)

  def secWebSocketVersion_=(v: String): Unit = set(Names.SEC_WEBSOCKET_VERSION, v)
  def secWebSocketVersion_=(v: Option[String]): Unit = set(Names.SEC_WEBSOCKET_VERSION, v)

  def server_=(v: String): Unit = set(Names.SERVER, v)
  def server_=(v: Option[String]): Unit = set(Names.SERVER, v)

  def setCookie_=(v: Traversable[String]): Unit = setAll(Names.SET_COOKIE, v)
  def setCookie_=(v: Option[Traversable[String]]): Unit = setAll(Names.SET_COOKIE, v)

  def setCookies_=(v: Traversable[Cookie]): Unit = setCookie = if (v.nonEmpty) Some(v.toSeq.map{ c: Cookie => ServerCookieEncoder.LAX.encode(c.toNettyCookie) }) else None
  def setCookies_=(v: Option[Traversable[Cookie]]): Unit = setCookies = v.getOrElse{ Nil }

  def setCookie2_=(v: Traversable[String]): Unit = setAll(Names.SET_COOKIE2, v)
  def setCookie2_=(v: Option[Traversable[String]]): Unit = setAll(Names.SET_COOKIE2, v)

  def te_=(v: String): Unit = set(Names.TE, v)
  def te_=(v: Option[String]): Unit = set(Names.TE, v)

  def trailer_=(v: String): Unit = set(Names.TRAILER, v)
  def trailer_=(v: Option[String]): Unit = set(Names.TRAILER, v)

  def transferEncoding_=(v: String): Unit = set(Names.TRANSFER_ENCODING, v)
  def transferEncoding_=(v: Option[String]): Unit = set(Names.TRANSFER_ENCODING, v)

  def upgrade_=(v: String): Unit = set(Names.UPGRADE, v)
  def upgrade_=(v: Option[String]): Unit = set(Names.UPGRADE, v)

  def userAgent_=(v: String): Unit = set(Names.USER_AGENT, v)
  def userAgent_=(v: Option[String]): Unit = set(Names.USER_AGENT, v)

  def vary_=(v: String): Unit = set(Names.VARY, v)
  def vary_=(v: Option[String]): Unit = set(Names.VARY, v)

  def via_=(v: String): Unit = set(Names.VIA, v)
  def via_=(v: Option[String]): Unit = set(Names.VIA, v)

  def warning_=(v: String): Unit = set(Names.WARNING, v)
  def warning_=(v: Option[String]): Unit = set(Names.WARNING, v)

  def webSocketLocation_=(v: String): Unit = set(Names.WEBSOCKET_LOCATION, v)
  def webSocketLocation_=(v: Option[String]): Unit = set(Names.WEBSOCKET_LOCATION, v)

  def webSocketOrigin_=(v: String): Unit = set(Names.WEBSOCKET_ORIGIN, v)
  def webSocketOrigin_=(v: Option[String]): Unit = set(Names.WEBSOCKET_ORIGIN, v)

  def webSocketProtocol_=(v: String): Unit = set(Names.WEBSOCKET_PROTOCOL, v)
  def webSocketProtocol_=(v: Option[String]): Unit = set(Names.WEBSOCKET_PROTOCOL, v)

  def wwwAuthenticate_=(v: String): Unit = set(Names.WWW_AUTHENTICATE, v)
  def wwwAuthenticate_=(v: Option[String]): Unit = set(Names.WWW_AUTHENTICATE, v)


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
  // Authorization
  //

  def basicAuthorization_=(userPass: (String,String)): Unit = {
    val (user, pass) = userPass
    Headers.makeBasicAuthorization(user, pass)
  }

  //
  // Private Helpers
  //

  private def makeContentDispositionAttachmentWithFileName(v: String): String = {
    val escapedName: String = v.replace("\"", "\\\"") // Replace " with \"
    s"""attachment; filename="$escapedName""""
  }
}