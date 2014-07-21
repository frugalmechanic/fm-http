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
package fm.netty

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}
import io.netty.util.concurrent.{Promise => NettyPromise}

/**
 * Wraps Netty's Promise as a Scala Promise
 */
final case class NettyPromiseWrapper[T](val p: NettyPromise[T]) extends Promise[T] {
  def future: Future[T] = NettyFutureWrapper(p)
  
  def isCompleted: Boolean = p.isDone()
  
  def tryComplete(result: Try[T]): Boolean = result match {
    case Success(value) => p.trySuccess(value)
    case Failure(ex)    => p.tryFailure(ex) 
  }
}