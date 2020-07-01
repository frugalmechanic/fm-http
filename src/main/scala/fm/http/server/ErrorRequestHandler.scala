/*
 * Copyright 2020 Frugal Mechanic (http://frugalmechanic.com)
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

object ErrorRequestHandler {
  // For legacy API support
  // TODO: consider changing this to take a Request => Response which would ensure there is no need for an ExecutionContext
  implicit def requestHandlerToErrorRequestHandler(f: Request => Future[Response]): ErrorRequestHandler = new ErrorRequestHandler {
    override def apply(request: Request, ex: Throwable)(implicit executor: ExecutionContext): Future[Response] = f(request)
  }

  // For legacy API Support
  implicit def toErrorRequestHandler(f: (Request, Throwable) => Future[Response]): ErrorRequestHandler = new ErrorRequestHandler {
    override def apply(request: Request, ex: Throwable)(implicit executor: ExecutionContext): Future[Response] = f(request, ex)
  }

  implicit def toErrorRequestHandler(f: (Request, Throwable, ExecutionContext) => Future[Response]): ErrorRequestHandler = new ErrorRequestHandler {
    override def apply(request: Request, ex: Throwable)(implicit executor: ExecutionContext): Future[Response] = f(request, ex, executor)
  }
}

trait ErrorRequestHandler {
  def apply(request: Request, ex: Throwable)(implicit executor: ExecutionContext): Future[Response]
}
