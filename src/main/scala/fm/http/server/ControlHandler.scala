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
import fm.http.Status
import scala.concurrent.Future

object ControlHandler {
  private def Ok = Future.successful(Response(Status.OK, "ok"))
  private def Enabled = Future.successful(Response(Status.OK, "enabled"))
  private def Disabled = Future.successful(Response(Status.SERVICE_UNAVAILABLE, "disabled"))
  private def NotAuthorized = Future.successful(Response(Status.FORBIDDEN, "invalid authorization"))
}

final case class ControlHandler(server: HttpServer, authKey: String) extends DefaultRequestRouter with Logging {
  import ControlHandler._
  import RouteMatchers._
  
  /**
   * This controls whether a "GET /_ping" returns a "200 OK" or a "503 Service Unavailable" response
   */
  @volatile var enabled: Boolean = false
  
  private def authorized(request: Request): Boolean = {
    // We don't allow non-localhost requests
    if (request.remoteIp.isNotLocalhost) return false
    
    // TODO: maybe make this something fancier (maybe using the MessageCrypto class once it's moved out of the internal repository)
    val res: Boolean = request.params.getFirst("key").exists{ _ === authKey }
    if (!res) logger.error("Unauthorized ControlHandler Access Attempt: "+request)
    res
  }
  
  protected val handler: PartialFunction[Request, Future[Response]] = (request: Request) => request match {
    case GET("/_alive")    => Ok
    case GET("/_ping")     => if (enabled) Ok else Disabled
    case GET("/_shutdown") => if (authorized(request)) { server.shutdown(); Ok       } else NotAuthorized
    case GET("/_enable")   => if (authorized(request)) { enabled = true;    Ok       } else NotAuthorized
    case GET("/_disable")  => if (authorized(request)) { enabled = false;   Disabled } else NotAuthorized
  }
}
