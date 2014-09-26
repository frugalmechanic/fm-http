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
package fm.http

import scala.concurrent.Future

package object server extends fm.netty.PackageImplicits {
  type RequestHandler = Request => Future[Response]
  
  final implicit class RichRequestHandler(val handler: RequestHandler) extends AnyVal {
    def withFilter(filter: RequestFilter): RequestHandler = FilteredRequestHandler(handler, filter)
    def withFilter(filter: Option[RequestFilter]): RequestHandler = if (filter.isDefined) withFilter(filter.get) else handler
    
    def withFilters(filters: TraversableOnce[RequestFilter]): RequestHandler = filters.foldRight(handler){ (filter, newHandler) => newHandler.withFilter(filter) }
  }
}
