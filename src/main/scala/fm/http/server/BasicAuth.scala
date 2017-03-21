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
import fm.common.Implicits._
import fm.http.{Headers, Status}
import java.nio.charset.StandardCharsets
import io.netty.handler.codec.http.HttpHeaders
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.matching.Regex

object BasicAuth {
  private val BasicAuthHeader: Regex = """Basic (.+)""".r
  private val BasicAuthSplit: Regex = """(.+):(.+)""".r

  private final case class StaticUsersBasicAuth (realm: String, users: Map[String, String]) extends BasicAuth {
    def isAuthorized(user: String, pass: String): Future[Boolean] = Future.successful(users.get(user).map{ _ === pass }.getOrElse(false))
  }

  // Performs Basic Authentication using the given Map of Username -> Plaintext Password before calling the passed in handler
  def apply(realm: String, users: Map[String, String]): BasicAuth = StaticUsersBasicAuth(realm, users)
}

/**
 * Mostly used the Wikipedia page as references:
 * http://en.wikipedia.org/wiki/Basic_access_authentication
 */
trait BasicAuth extends Auth with Logging {
  import BasicAuth._

  // Implemented by child classes
  protected def realm: String
  protected def isAuthorized(user: String, pass: String): Future[Boolean]

  protected def requireAuthImpl(request: Request)(action: => Future[Response]): Future[Response] = {
    isValid(request).flatMap{ valid: Boolean =>
      if (valid) action
      else Future.successful(Response(Status.UNAUTHORIZED, Headers(HttpHeaders.Names.WWW_AUTHENTICATE -> s"""Basic realm="$realm"""")))
    }
  }

  private def isValid(request: Request): Future[Boolean] = try {
    val auth: String = request.headers.authorization.getOrElse{ return Future.successful(false) }

    auth match {
      case BasicAuthHeader(encoded) =>
        new String(Base64.decode(encoded), StandardCharsets.ISO_8859_1) match {
          case BasicAuthSplit(user, pass) => isAuthorized(user, pass)
          case _ => Future.successful(false)
        }

      case _ => Future.successful(false)
    }
  } catch {
    case ex: Exception =>
      logger.warn("Caught Exception Performing Basic Auth", ex)
      Future.successful(false)
  }
}