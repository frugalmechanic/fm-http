package fm.http.client

import fm.common.DigestUtils
import fm.common.Implicits._
import fm.http.Headers
import java.security.SecureRandom

final class DigestAuthHttpClient(user: String, pass: String, val client: HttpClient) extends AuthHttpClient {
  private[this] val secureRandom: SecureRandom = new SecureRandom()
  
  private[this] var realm: String = null
  private[this] var nonce: String = null
  private[this] var algorithm: String = null
  private[this] var qop: String = null
  private[this] var opaque: String = null
  private[this] var nc: Int = 0
  
  protected def makeAuthorization(request: Request): Option[String] = synchronized {
    if (realm.isBlank || nonce.isBlank || opaque.isBlank) return None
    
    nc += 1
    val ncHex: String = nc.toHexString.lPad(8,'0') // e.g. 00000001
    
    val cnonce: String = DigestUtils.md5Hex(s"$realm:${System.currentTimeMillis}:${secureRandom.nextLong}")

    var ha1: String = DigestUtils.md5Hex(s"$user:$realm:$pass")
    
    // Only look at the algorithm if the qop is non-blank
    if (qop.isNotBlank) algorithm match {
      case null | "MD5" => // ha1 is used as-is
      case "MD5-sess"   => ha1 = DigestUtils.md5Hex(s"$ha1:$nonce:$cnonce")
      case _            => return None // Not sure how to authenticate
    }
    
    val ha2: String = qop match {
      case null | "auth" => DigestUtils.md5Hex(request.method.name+":"+request.requestURI)
      case "auth-int"    => return None // auth-int not implemented
      case _             => return None // unknown value for qop
    }
    
    val response: String = qop match {
      case null                => DigestUtils.md5Hex(s"$ha1:$nonce:$ha2")
      case "auth" | "auth-int" => DigestUtils.md5Hex(s"$ha1:$nonce:$ncHex:$cnonce:$qop:$ha2")
      case _                   => return None // unknown value for qop
    }
    
    val params = Vector.newBuilder[(String,String)]
    
    params += "username" -> user
    params += "realm" -> realm
    params += "uri" -> request.requestURI
    params += "nonce" -> nonce
    params += "opaque" -> opaque
    params += "response" -> response
    
    if (qop.isNotBlank) {
      params += "qop" -> qop
      params += "cnonce" -> cnonce
      params += "nc" -> ncHex
    }
    
    Headers.makeDigestAuthorization(params.result).toBlankOption
  }
  
  protected def makeAuthorization(request: Request, response: Response): Option[String] = synchronized {
    val params: Map[String,String] = response.headers.digestAuthParams.getOrElse{ return None }
    
    // Update our params from the response
    realm = params.getOrElse("realm", null)
    nonce = params.getOrElse("nonce", null)
    algorithm = params.getOrElse("algorithm", null)
    qop = params.getOrElse("qop", null)
    opaque = params.getOrElse("opaque", null)
    nc = 0
    
    makeAuthorization(request)
  }
}