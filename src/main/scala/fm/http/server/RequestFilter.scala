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

import scala.concurrent.Future

object RequestFilter {
  object empty extends RequestFilter {
    def handle(request: Request, handler: RequestHandler): Future[Response] = handler(request) 
  }
}

trait RequestFilter {
  def handle(request: Request, handler: RequestHandler): Future[Response]
  final def andThen(that: RequestFilter): AndThenRequestFilter = AndThenRequestFilter(this, that)
}

final case class AndThenRequestFilter(a: RequestFilter, b: RequestFilter) extends RequestFilter {
  def handle(request: Request, handler: RequestHandler): Future[Response] = {
    a.handle(request, (request: Request) => b.handle(request, handler))
  }
}