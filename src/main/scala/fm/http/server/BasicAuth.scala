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

import fm.common.Logging
import fm.common.Implicits._
import fm.http.{Headers, Status}
import io.netty.handler.codec.http.HttpHeaderNames
import scala.concurrent.{ExecutionContext, Future}

object BasicAuth {
  private final case class StaticUsersBasicAuth (realm: String, users: Map[String, String]) extends BasicAuth {
    def isAuthorized(user: String, pass: String): Future[Boolean] = Future.successful(users.get(user).map{ _ === pass }.getOrElse(false))
  }

  /** Performs Basic Authentication using the given Map of Username -> Plaintext Password before calling the passed in handler */
  def apply(realm: String, users: Map[String, String]): BasicAuth = StaticUsersBasicAuth(realm, users)
}

/**
 * Mostly used the Wikipedia page as references:
 * http://en.wikipedia.org/wiki/Basic_access_authentication
 */
trait BasicAuth extends Auth with Logging {
  // Implemented by child classes
  def realm: String
  def isAuthorized(user: String, pass: String): Future[Boolean]

  final protected def requireAuthImpl(request: Request)(action: => Future[Response])(implicit executor: ExecutionContext): Future[Response] = {
    isAuthorized(request).flatMap{ valid: Boolean =>
      if (valid) action
      else Future.successful(Response(Status.UNAUTHORIZED, Headers(HttpHeaderNames.WWW_AUTHENTICATE -> s"""Basic realm="$realm"""")))
    }
  }

  final def isAuthorized(request: Request): Future[Boolean] = {
    val auth: String = request.headers.authorization.getOrElse{ return Future.successful(false) }

    Headers.parseBasicAuthorization(auth) match {
      case Some((user,pass)) => isAuthorized(user, pass)
      case None => Future.successful(false)
    }
  }
}