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

import fm.common.Implicits._
import scala.concurrent.{ExecutionContext, Future}

/**
 * If the request host doesn't match the host param then redirect
 */
final case class DomainRedirectFilter(host: String) extends RequestFilter {
  
  def handle(request: Request, handler: RequestHandler)(implicit executor: ExecutionContext): Future[Response] = {
    val requestHost: String = request.headers.host.getOrElse("")
    
    if (host === requestHost) {
      handler(request)
    } else {
      // Use a scheme-relative URL
      val scheme: String = "//"
      
      // If the URI is just "/" then we use a blank string otherwise we require that there is a leading "/".
      // e.g. if the uri is "/" then we redirect to just //$host
      val uri: String = request.uri.stripLeading("/").toBlankOption.map{ _.requireLeading("/") }.getOrElse("")
      
      Future.successful(Response.MovedPermanently(s"$scheme$host$uri"))
    }
  }
  
}