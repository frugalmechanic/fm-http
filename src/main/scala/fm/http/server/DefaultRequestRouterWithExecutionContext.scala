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

/**
 * This provides a base for both a RequestRouter and RequestHandler that is implemented by using a PartialFunction.
 *
 * Note: If you do not need a PartialFunction that takes an Execution context then see the DefaultRequestRouter
 *       version of this.
 */
abstract class DefaultRequestRouterWithExecutionContext extends RequestRouterBase with RequestRouterAndHandler {
  protected val handler: PartialFunction[Request, ExecutionContext => Future[Response]]

  private[this] val thisHandler: Some[RequestHandler] = Some(this)

  final override def lookup(request: Request): Option[RequestHandler] = {
    if (handler.isDefinedAt(request)) thisHandler else None
  }

  final override def apply(request: Request)(implicit executor: ExecutionContext): Future[Response] = {
    handler(request)(executor)
  }
}