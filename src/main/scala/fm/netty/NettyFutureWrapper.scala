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

import java.util.concurrent.TimeoutException
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import io.netty.util.concurrent.{Future => NettyFuture, GenericFutureListener}

object NettyFutureWrapper {
  private class Listener[T, U](func: Try[T] => U) extends GenericFutureListener[NettyFuture[T]] {
    def operationComplete(f: NettyFuture[T]): Unit = {
      if (f.isSuccess()) func(Success(f.get)) else func(Failure(f.cause))
    }
  }
}

/**
 * Wraps Netty's Future as a Scala Future while ensuring that the ExecutionContext
 * is Netty's NioEventLoopGroup
 */
final case class NettyFutureWrapper[T](val f: NettyFuture[T]) extends Future[T] {
  
  def isCompleted: Boolean = f.isDone
  
  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext): Unit = {
    // Note: calling flatMap will trigger this exception since it uses some Internal Executor in the Future class.  Comment out if you need to use flatMap
    //require(executor.isInstanceOf[NettyExecutionContext], "Expected the ExecutionContext to be a NettyExecutionContext but got: "+executor)
    f.addListener(new NettyFutureWrapper.Listener(func))
  }
  
  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = atMost match {
    case Duration.Undefined => throw new IllegalArgumentException("Cannot use Duration.Undefined")
    case Duration.Inf => f.await(); this 
    case _ => if (f.await(atMost.length, atMost.unit)) this else throw new TimeoutException()
  }
  
  def result(atMost: Duration)(implicit permit: CanAwait): T = {
    ready(atMost)
    assert(f.isDone)
    if (f.isSuccess) f.get else throw f.cause
  }
  
  def value: Option[Try[T]] = if (f.isDone) Some(if (f.isSuccess) Success(f.get) else Failure(f.cause)) else None
}