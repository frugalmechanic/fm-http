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

import fm.common.{Base64, Logging}
import fm.http.{Headers, Status}
import io.netty.handler.codec.http.HttpHeaders
import scala.concurrent.Future
import scala.util.matching.Regex

object BasicAuth {
  private val BasicAuthHeader: Regex = """Basic (.+)""".r
  private val BasicAuthSplit: Regex = """(.+):(.+)""".r
}

/**
 * Performs Basic Authentication using the given Map of Username -> Plaintext Password before calling the passed in handler
 * 
 * Mostly used the Wikipedia page as references:
 * http://en.wikipedia.org/wiki/Basic_access_authentication
 */
final case class BasicAuth(realm: String, users: Map[String, String]) extends Auth with Logging {
  import BasicAuth._
  
  protected def requireAuthImpl(request: Request)(action: => Future[Response]): Future[Response] = {
    if (isValid(request)) action else {
      Future.successful(Response(Status.UNAUTHORIZED, Headers(HttpHeaders.Names.WWW_AUTHENTICATE -> s"""Basic realm="$realm"""")))
    }
  }
  
  private def isValid(request: Request): Boolean = try {
    val auth: String = request.headers.authorization.getOrElse{ return false } 
    
    auth match {
      case BasicAuthHeader(encoded) => new String(Base64.decode(encoded)) match {
        case BasicAuthSplit(user, pass) => users.get(user).map{ _ == pass }.getOrElse(false)
        case _ => false
      }
      case _ => false
    }
  } catch {
    case ex: Exception => 
      logger.warn("Caught Exception Performing Basic Auth", ex)
      false
  }
}