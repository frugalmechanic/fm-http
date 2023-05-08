/*
 * Copyright 2015 Frugal Mechanic (http://frugalmechanic.com)
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

import java.util.IdentityHashMap

object RequestLocal {
  def apply[T](): RequestLocal[T] = new RequestLocal()
}

/**
 * Like a ThreadLocal except for Requests
 *
 * You can either instantiate this directly or extend it:
 *
 *   {{{ val username = new RequestLocal[String] }}}
 *
 *   {{{ object username extends RequestLocal[String] }}}
 */
class RequestLocal[T] {
  final def apply()(implicit request: Request): T = {
    get.getOrElse{ throw new NoSuchElementException }
  }

  final def getOrElseUpdate(value: => T)(implicit request: Request): T = {
    val res: Option[T] = getWithoutInitialValue
    if (res.isDefined) return res.get

    val v: T = value
    if (null != v) set(v)
    v
  }

  final def getOrElseUpdate(value: => Option[T])(implicit request: Request): Option[T] = {
    val res: Option[T] = getWithoutInitialValue
    if (res.isDefined) return res

    val v: Option[T] = value
    if (v.isDefined) set(v.get)
    v
  }

  private def getWithoutInitialValue(implicit request: Request): Option[T] = {
    if (isRequestLocalMapNull) return None

    val res: AnyRef = requestLocalMap.get(this)
    if (null == res) None else Some(res.asInstanceOf[T])
  }

  final def get(implicit request: Request): Option[T] = {
    getOrElseUpdate{ initialValue }
  }

  final def hasValue(implicit request: Request): Boolean = request.synchronized {
    if (isRequestLocalMapNull) false
    else requestLocalMap.containsKey(this)
  }

  final def setIfNotExists(value: => T)(implicit request: Request): Unit = request.synchronized {
    if (!hasValue) set(value)
  }

  final def set(value: Option[T])(implicit request: Request): Unit = request.synchronized {
    if (value.isDefined) set(value.get)
    else remove()
  }

  final def set(value: T)(implicit request: Request): Unit = request.synchronized {
    if (null == value) {
      // We do not allow null values so treat those as removals
      remove()
    } else {
      initRequestLocalMap()
      requestLocalMap.put(this, value.asInstanceOf[AnyRef])
    }
  }

  final def remove()(implicit request: Request): Unit = request.synchronized {
    if (isRequestLocalMapNull) return // Nothing to do
    requestLocalMap.remove(this)
  }

  private[this] def requestLocalMap(implicit request: Request): IdentityHashMap[RequestLocal[_],AnyRef] = {
    request.requestLocalMap
  }

  // Helpers for the above methods that assume they are being run in synchronized blocks
  private[this] def isRequestLocalMapNull(implicit request: Request): Boolean = {
    null == request.requestLocalMap
  }

  private[this] def initRequestLocalMap()(implicit request: Request): Unit = {
    if (isRequestLocalMapNull) request.requestLocalMap = new IdentityHashMap()
  }

  /**
   * Override this is you want to have a default value that is set on first access if set() hasn't been called
   */
  protected def initialValue(implicit request: Request): Option[T] = None  
}
