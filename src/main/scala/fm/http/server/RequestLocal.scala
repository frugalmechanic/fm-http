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

/**
 * Like a ThreadLocal except for Requests
 */
trait RequestLocal[T] {
  def get(implicit request: Request): Option[T] = synchronized {
    val res: AnyRef = request.requestLocalMap.get(this)
    
    if (null != res) return Some(res.asInstanceOf[T])
    
    val iv: Option[T] = initialValue
    if (iv.isDefined) set(iv.get)
    iv
  }
  
  def apply(implicit request: Request): T = get.getOrElse{ throw new NoSuchElementException }
  
  def set(value: Option[T])(implicit request: Request): Unit = if (value.isDefined) set(value.get) else remove
  
  def set(value: T)(implicit request: Request): Unit = synchronized {
    request.requestLocalMap.put(this, value.asInstanceOf[AnyRef])
  }
  
  def remove(implicit request: Request): Unit = synchronized {
    request.requestLocalMap.remove(this)
  }
  
  /**
   * Override this is you want to have a default value that is set on first access if set() hasn't been called
   */
  protected def initialValue(implicit request: Request): Option[T] = None  
}
