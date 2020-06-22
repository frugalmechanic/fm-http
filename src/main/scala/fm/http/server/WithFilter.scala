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

trait WithFilter[+A <: WithFilter[A]] { self: A =>
  /**
   * Run any RequestHandlers returned by this RequestRouter through a RequestFilter
   */
  def withFilter(filter: RequestFilter): A

  /**
   * Run any RequestHandlers returned by this RequestRouter through an optional RequestFilter
   */
  final def withFilter(filter: Option[RequestFilter]): A = {
    if (filter.isDefined) withFilter(filter.get) else this
  }

  /**
   * Run any RequestHandlers returned by this RequestRouter through a sequence of RequestFilter
   */
  final def withFilters(filters: TraversableOnce[RequestFilter]): A = {
    filters.foldRight(this){ (filter: RequestFilter, newHandler: A) => newHandler.withFilter(filter) }
  }
}
