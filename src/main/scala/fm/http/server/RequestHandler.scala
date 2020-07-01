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

import scala.concurrent.{ExecutionContext, Future}

object RequestHandler {
  // For legacy API support.
  // TODO: consider changing this to take a Request => Response which would ensure there is no need for an ExecutionContext
  implicit def toRequestHandler(f: Request => Future[Response]): RequestHandler = new RequestHandler {
    def apply(request: Request)(implicit executor: ExecutionContext): Future[Response] = f(request)
  }

  implicit def toRequestHandler(f: (Request, ExecutionContext) => Future[Response]): RequestHandler = new RequestHandler {
    def apply(request: Request)(implicit executor: ExecutionContext): Future[Response] = f(request, executor)
  }

  def constant(response: Response) = new RequestHandler {
    private[this] val res: Future[Response] = Future.successful(response)
    def apply(request: Request)(implicit executor: ExecutionContext): Future[Response] = res
  }
}

trait RequestHandler extends WithFilter[RequestHandler] {
  def apply(request: Request)(implicit executor: ExecutionContext): Future[Response]

  override def withFilter(filter: RequestFilter): RequestHandler = FilteredRequestHandler(this, filter)
}