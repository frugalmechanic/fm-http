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
package fm.http.server

import com.frugalmechanic.optparse.OptParse
import fm.common.{DigestUtils, Logging, Crypto, StacklessException}
import fm.http.{Headers, Status}
import io.netty.handler.codec.http.HttpHeaders
import java.security.SecureRandom
import scala.util.matching.Regex
import scala.concurrent.Future

object DigestAuth {
  private val DigestAuthHeader: Regex = """Digest (.+)""".r
  private val DigestAuthParam: Regex = """(\w+)=(?:"([^"]+)"|([^,]+)),?""".r
  
  private case object Stale extends StacklessException
  
  def digestHash(realm: String, username: String, password: String): String = {
    DigestUtils.md5Hex(username+":"+realm+":"+password)
  }
  
  object CLIOptions extends OptParse {
    val realm = StrOpt(desc="Realm")
    val user = StrOpt(desc="Username")
    val pass = StrOpt(desc="Pass")
  }

  /**
   * For generating DIGEST hashes (like Apache's htdigest utility)
   */
  def main(args: Array[String]): Unit = {
    CLIOptions.parse(args)

    val realm: String = CLIOptions.realm.getOrElse {
      print("Enter Realm: ")
      System.console.readLine().trim()
    }
    
    val user: String = CLIOptions.user.getOrElse {
      print("Enter User: ")
      System.console.readLine().trim()
    }

    val pass: String = CLIOptions.pass.getOrElse {
      print("Enter Password: ")
      val pw1 = new String(System.console.readPassword())

      print("Again: ")
      val pw2 = new String(System.console.readPassword())

      require(pw1 == pw2, "Passwords do not match!")
      pw1
    }

    val hash: String = digestHash(realm=realm, username=user, password=pass)
    
    println("Realm: "+realm)
    println("User: "+user)
    println("Digest Hash: "+hash)
  }
}

/**
 * Implements DIGEST HTTP Authentication
 *
 * Mostly used the Wikipedia pages as references:
 * http://en.wikipedia.org/wiki/Digest_access_authentication
 * 
 * NOTE: Use at your own risk.  We make no claim that any of this Crypto code is correct.
 */
final case class DigestAuth(
    /**
     * The realm to use
     */
    realm: String,
    /**
     * Map of Usernames -> (Plaintext passwords or hashes based on the digestHash method in the DigestAuth object)
     */
    users: Map[String, String],
    /**
     * A value to prefix to the nonce.  Can be anything.
     */
    noncePrefix: String,
    /** 
     * The Base64 encoded 256-bit encryption key to use for encrypting the opaque data. You
     * can use fm.common.Crypto.makeRandomKeyBase64() to generate a key.
     */
    base64EncryptionKey: String,
    /**
     * How long if the nonce good for?  After this amount of seconds the client's browser
     * will automatically re-authenticate using the updated nonce from the server.
     */
    expirationSeconds: Int = 300 // 5 minutes
) extends Auth with Logging {
  
  import DigestAuth._

  // Prefix this onto the nonce
  private[this] val DigestNoncePrefix: String = noncePrefix
  
  // Encrypt/Sign the opaque data using this
  private[this] val DigestOpaqueCrypto = Crypto.authenticatedCipherForBase64Key(base64EncryptionKey)

  // The nonce expires after this number of seconds.  After the expiration the "stale" flag is passed
  // back to the client which should allow the browser to automatically retry the request with
  // the updated nonce without prompting the user for their username/password.
  private[this] val DigestExpirationSeconds: Int = expirationSeconds
  
  private[this] val secureRandom: SecureRandom = new SecureRandom()

  protected def requireAuthImpl(request: Request)(action: => Future[Response]): Future[Response] = try {
    if (isValid(request)) action else response(isStale = false)
  } catch {
    case Stale => response(isStale = true)
  }
  
  private def response(isStale: Boolean): Future[Response] = {
    val nonce: String = DigestUtils.md5Hex(DigestNoncePrefix+System.currentTimeMillis.toString+secureRandom.nextLong.toString)
    val opaque: String = DigestOpaqueCrypto.encryptBase64String(System.currentTimeMillis+":"+nonce)

    val stale: Seq[(String,String)] = if (isStale) Seq("stale" -> "true") else Seq()

    val params: Seq[(String,String)] = Seq(
      "realm" -> realm,
      "qop" -> "auth",
      "nonce" -> nonce,
      "opaque" -> opaque
    )++stale

    val wwwAuthenticateValue: String = "Digest "+params.map{case (k,v) => k+"=\""+v+"\""}.mkString(", ")
    Future.successful(Response(Status.UNAUTHORIZED, Headers(HttpHeaders.Names.WWW_AUTHENTICATE -> wwwAuthenticateValue)))
  }
  
  def isValid(request: Request): Boolean = {
    val auth: String = request.headers.authorization.getOrElse{ return false } 
    
    auth match {
      case DigestAuthHeader(paramsStr) =>
        val params: Map[String, String] = Map(DigestAuthParam.findAllIn(paramsStr).matchData.map{ m => 
            val k: String = m.group(1)
            val v: String = if (null != m.group(2)) m.group(2) else m.group(3)
            (k,v)
          }.toSeq: _*)
          
       if (logger.isDebugEnabled) logger.debug(s"Request: ${request.method} ${request.uri}  Params: $params")
       
       isValid(request, params)
       
      case _ => false
    }
  }
  
  def isValid(request: Request, params: Map[String, String]): Boolean = {
    val pUsername = params.getOrElse("username", "")
    val pRealm = params.getOrElse("realm", "")
    val pNonce = params.getOrElse("nonce", "")
    val pClientNonce = params.getOrElse("cnonce", "")
    val pCount = params.getOrElse("nc", "")
    val pUri = params.getOrElse("uri", "")
    val pResponse = params.getOrElse("response", "")
    val pOpaque = params.getOrElse("opaque", "")
    val pQop = params.getOrElse("qop", "")
    
    def computeResponse(ha1: String): String = {
      val ha2: String = DigestUtils.md5Hex(request.method.name+":"+request.uri)
  
      pQop match {
        case "auth" => DigestUtils.md5Hex(ha1+":"+pNonce+":"+pCount+":"+pClientNonce+":auth:"+ha2)
        case ""     => DigestUtils.md5Hex(ha1+":"+pNonce+":"+ha2)
        case _      => ""
      }
    }
    
    // Some basic checks to ignore any obviously invalid requests
    if (!users.contains(pUsername) || realm != pRealm || pUri != request.uri) return false
    
    // Verify the opaque value was encrypted/signed by us
    val opaqueMsg: String = DigestOpaqueCrypto.tryDecryptBase64String(pOpaque).getOrElse{ return false }
    
    val Array(time,nonce) = opaqueMsg.split(':')
    
    // If the nonce doesn't match then return
    if (nonce != pNonce) return false
    
    // Force re-authorization after a certain amount of time
    val expirationTimeMillis = time.toLong+(DigestExpirationSeconds*1000)
    val isStale: Boolean = System.currentTimeMillis > expirationTimeMillis
    
    if (isStale) throw Stale
    
    // Allow the users map to work with both pre-computed hashes and plaintext passwords
    val ha1Precomputed: String = users(pUsername)
    val ha1Plaintext: String = digestHash(realm=realm, username=pUsername, password=users(pUsername))

    val res: Boolean = pResponse == computeResponse(ha1Precomputed) || pResponse == computeResponse(ha1Plaintext)

    if (!res && logger.isDebugEnabled) logger.debug("Response mismatch - Expected: "+computeResponse(ha1Precomputed)+" OR "+computeResponse(ha1Plaintext)+"  Got: "+pResponse)
    
    res
  }
}