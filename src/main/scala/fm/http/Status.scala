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

import fm.common.Implicits._
import io.netty.handler.codec.http.HttpResponseStatus

final class Status private (netty: HttpResponseStatus) {
  def code: Int = netty.code()
  def msg: String = netty.reasonPhrase()
  
  /** Is this a 200 OK Response? */
  def isOK: Boolean = netty === HttpResponseStatus.OK
  
  /** Is this a 401 Unauthorized Response? */
  def isUnauthorized: Boolean = netty === HttpResponseStatus.UNAUTHORIZED
  
  /** Is this a 403 Forbidden Response? */
  def isForbidden: Boolean = netty === HttpResponseStatus.FORBIDDEN
  
  /** If this a 301/302/303/307 response that should include a Location redirect? */
  def isRedirect: Boolean = netty match {
    case HttpResponseStatus.MOVED_PERMANENTLY => true  // 301
    case HttpResponseStatus.FOUND => true              // 302
    case HttpResponseStatus.SEE_OTHER => true          // 303
    case HttpResponseStatus.TEMPORARY_REDIRECT => true // 307
    case _ => false
  }

  override def toString(): String = s"fm.http.Status($code)"

  def toHttpResponseStatus: HttpResponseStatus = netty
  
  override def hashCode: Int = netty.hashCode()
  override def equals(other: Any): Boolean = other match {
    case s: Status => s.code === code
    case _ => false
  }
}

object Status {
  def apply(netty: HttpResponseStatus): Status = new Status(netty)
  def apply(code: Int): Status = new Status(HttpResponseStatus.valueOf(code))
  def apply(code: Int, msg: String): Status = new Status(new HttpResponseStatus(code, msg))
  
  // 1xx
  val CONTINUE = new Status(HttpResponseStatus.CONTINUE)
  val SWITCHING_PROTOCOLS = new Status(HttpResponseStatus.SWITCHING_PROTOCOLS)
  val PROCESSING = new Status(HttpResponseStatus.PROCESSING)
  
  // 2xx
  val OK = new Status(HttpResponseStatus.OK)
  val CREATED = new Status(HttpResponseStatus.CREATED)
  val ACCEPTED = new Status(HttpResponseStatus.ACCEPTED)
  val NON_AUTHORITATIVE_INFORMATION = new Status(HttpResponseStatus.NON_AUTHORITATIVE_INFORMATION)
  val NO_CONTENT = new Status(HttpResponseStatus.NO_CONTENT)
  val RESET_CONTENT = new Status(HttpResponseStatus.RESET_CONTENT)
  val PARTIAL_CONTENT = new Status(HttpResponseStatus.PARTIAL_CONTENT)
  val MULTI_STATUS = new Status(HttpResponseStatus.MULTI_STATUS)
  
  // 3xx
  val MULTIPLE_CHOICES = new Status(HttpResponseStatus.MULTIPLE_CHOICES)
  val MOVED_PERMANENTLY = new Status(HttpResponseStatus.MOVED_PERMANENTLY)
  val FOUND = new Status(HttpResponseStatus.FOUND)
  val SEE_OTHER = new Status(HttpResponseStatus.SEE_OTHER)
  val NOT_MODIFIED = new Status(HttpResponseStatus.NOT_MODIFIED)
  val USE_PROXY = new Status(HttpResponseStatus.USE_PROXY)
  val TEMPORARY_REDIRECT = new Status(HttpResponseStatus.TEMPORARY_REDIRECT)
  
  // 4xx
  val BAD_REQUEST = new Status(HttpResponseStatus.BAD_REQUEST)
  val UNAUTHORIZED = new Status(HttpResponseStatus.UNAUTHORIZED)
  val PAYMENT_REQUIRED = new Status(HttpResponseStatus.PAYMENT_REQUIRED)
  val FORBIDDEN = new Status(HttpResponseStatus.FORBIDDEN)
  val NOT_FOUND = new Status(HttpResponseStatus.NOT_FOUND)
  val METHOD_NOT_ALLOWED = new Status(HttpResponseStatus.METHOD_NOT_ALLOWED)
  val NOT_ACCEPTABLE = new Status(HttpResponseStatus.NOT_ACCEPTABLE)
  val PROXY_AUTHENTICATION_REQUIRED = new Status(HttpResponseStatus.PROXY_AUTHENTICATION_REQUIRED)
  val REQUEST_TIMEOUT = new Status(HttpResponseStatus.REQUEST_TIMEOUT)
  val CONFLICT = new Status(HttpResponseStatus.CONFLICT)
  val GONE = new Status(HttpResponseStatus.GONE)
  val LENGTH_REQUIRED = new Status(HttpResponseStatus.LENGTH_REQUIRED)
  val PRECONDITION_FAILED = new Status(HttpResponseStatus.PRECONDITION_FAILED)
  val REQUEST_ENTITY_TOO_LARGE = new Status(HttpResponseStatus.REQUEST_ENTITY_TOO_LARGE)
  val REQUEST_URI_TOO_LONG = new Status(HttpResponseStatus.REQUEST_URI_TOO_LONG)
  val UNSUPPORTED_MEDIA_TYPE = new Status(HttpResponseStatus.UNSUPPORTED_MEDIA_TYPE)
  val REQUESTED_RANGE_NOT_SATISFIABLE = new Status(HttpResponseStatus.REQUESTED_RANGE_NOT_SATISFIABLE)
  val EXPECTATION_FAILED = new Status(HttpResponseStatus.EXPECTATION_FAILED)
  val UNPROCESSABLE_ENTITY = new Status(HttpResponseStatus.UNPROCESSABLE_ENTITY)
  val LOCKED = new Status(HttpResponseStatus.LOCKED)
  val FAILED_DEPENDENCY = new Status(HttpResponseStatus.FAILED_DEPENDENCY)
  val UNORDERED_COLLECTION = new Status(HttpResponseStatus.UNORDERED_COLLECTION)
  val UPGRADE_REQUIRED = new Status(HttpResponseStatus.UPGRADE_REQUIRED)
  val PRECONDITION_REQUIRED = new Status(HttpResponseStatus.PRECONDITION_REQUIRED)
  val TOO_MANY_REQUESTS = new Status(HttpResponseStatus.TOO_MANY_REQUESTS)
  val REQUEST_HEADER_FIELDS_TOO_LARGE = new Status(HttpResponseStatus.REQUEST_HEADER_FIELDS_TOO_LARGE)
  
  // 5xx
  val INTERNAL_SERVER_ERROR = new Status(HttpResponseStatus.INTERNAL_SERVER_ERROR)
  val NOT_IMPLEMENTED = new Status(HttpResponseStatus.NOT_IMPLEMENTED)
  val BAD_GATEWAY = new Status(HttpResponseStatus.BAD_GATEWAY)
  val SERVICE_UNAVAILABLE = new Status(HttpResponseStatus.SERVICE_UNAVAILABLE)
  val GATEWAY_TIMEOUT = new Status(HttpResponseStatus.GATEWAY_TIMEOUT)
  val HTTP_VERSION_NOT_SUPPORTED = new Status(HttpResponseStatus.HTTP_VERSION_NOT_SUPPORTED)
  val VARIANT_ALSO_NEGOTIATES = new Status(HttpResponseStatus.VARIANT_ALSO_NEGOTIATES)
  val INSUFFICIENT_STORAGE = new Status(HttpResponseStatus.INSUFFICIENT_STORAGE)
  val NOT_EXTENDED = new Status(HttpResponseStatus.NOT_EXTENDED)
  val NETWORK_AUTHENTICATION_REQUIRED = new Status(HttpResponseStatus.NETWORK_AUTHENTICATION_REQUIRED)
}
