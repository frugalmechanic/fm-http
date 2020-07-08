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
package fm.http.client

import fm.common.Logging
import fm.common.Implicits._
import java.util.{Deque, Queue}
import java.util.concurrent.{ConcurrentLinkedDeque, LinkedBlockingQueue}
import io.netty.channel.Channel
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object ChannelPool {
  private case class IdleChannel(channel: Channel, lastActivity: Long)

  // After this many exceptions we will disable the pool and assume they don't support keep-alive or have a very short
  // keep alive time.
  private[http] val ExceptionCountThreshold: Int = 3 // Arbitrarily chosen value
}

final case class ChannelPool(label: String, newChannel: ChannelPool => Future[Channel], limit: Int, maxQueueSize: Int, connectionMaxIdleMillis: Long, poolMaxIdleMillis: Long)(implicit executionCtx: ExecutionContext) extends Logging {
  import ChannelPool.IdleChannel
  
  private[this] var count: Int = 0
  private[http] var exceptionCount: Int = 0
  private[http] var poolDisabled: Boolean = false
  private[this] val waitingQueue: Queue[Promise[Channel]] = new LinkedBlockingQueue(maxQueueSize)
  private[this] val idleChannels: Deque[IdleChannel] = new ConcurrentLinkedDeque()
  private[this] var lastActivity: Long = System.currentTimeMillis()

  def isEmptyAndIdle: Boolean = synchronized {
    isEmpty && lastActivity < System.currentTimeMillis() - poolMaxIdleMillis
  }

  private def isEmpty: Boolean = synchronized{
    idleChannels.isEmpty && waitingQueue.isEmpty
  }

  def closeIdleChannels(): Unit = synchronized {
    if (idleChannels.isEmpty()) return

    trace("closeIdleChannels()")

    val oldestAge: Long = System.currentTimeMillis() - connectionMaxIdleMillis

    val it: java.util.Iterator[IdleChannel] = idleChannels.iterator()

    while (it.hasNext) {
      val idle: IdleChannel = it.next()
      if (idle.lastActivity < oldestAge) {
        idle.channel.close()
        it.remove()
      }
    }
  }

  def markException(): Unit = synchronized {
    trace(s"markException() - exceptionCount: $exceptionCount")
    exceptionCount += 1

    if (exceptionCount >= ChannelPool.ExceptionCountThreshold && !poolDisabled) {
      poolDisabled = true
      error("ChannelPool for has been disabled due to too many exceptions")

      // Close all idle channels which will prevent us from re-using any more connections
      val it: java.util.Iterator[IdleChannel] = idleChannels.iterator()

      while (it.hasNext) {
        val idle: IdleChannel = it.next()
        idle.channel.close()
        it.remove()
      }
    }
  }

  def checkout(): Future[Channel] = synchronized {
    trace("checkout()")

    lastActivity = System.currentTimeMillis()

    var idle: IdleChannel = null

    // Keep polling IdleChannels until we either get a null or an active channel
    do {
      idle = idleChannels.poll()
    } while (null != idle && !idle.channel.isActive)
    
    if (null != idle) {
      trace(s"checkout() - Using Idle: $idle")
      // Use up an idle connections
      Future.successful(idle.channel)
    } else if (count < limit) {
      trace(s"checkout() - Using New Connection")
      
      // Create a new connection
      count += 1
      val res: Future[Channel] = newChannel(this)
      
      res.onComplete {
        case Success(ch) => ch.closeFuture().onComplete{ case _ => remove(ch) }
        case Failure(_) => onRemove()
      }
      
      res
    } else {
      trace(s"checkout() - Queueing")
      val p: Promise[Channel] = Promise()
      if (!waitingQueue.offer(p)) {
        val msg: String = s"ChannelPool ($label) waitingQueue is full!"
        logger.error(msg)
        p.failure(new Exception(msg))
      }
      p.future
    }
  }
  
  /**
   * Release a channel back to the queue
   */
  def release(ch: Channel): Unit = synchronized {
    trace(s"release($ch)")
    
    if (!ch.isActive()) {
      logger.warn("release() called on inActive Channel: "+ch)
      return
    }

    // If we don't want to use the pool then just close the channel which will trigger remove() and onRemove()
    if (poolDisabled) {
      ch.close()
      return
    }
    
    var doRetry: Boolean = false
    
    do {
      doRetry = false
    
      val waiting: Promise[Channel] = waitingQueue.poll()
      
      if (null == waiting) idleChannels.push(IdleChannel(ch, System.currentTimeMillis()))
      else if (!waiting.trySuccess(ch)) doRetry = true
      
    } while (doRetry)
  }
  
  private def remove(ch: Channel): Unit = synchronized {
    trace(s"remove($ch)")
    
    var done: Boolean = false
    val it: java.util.Iterator[IdleChannel] = idleChannels.iterator()
    
    while (it.hasNext() && !done) {
      val idle: IdleChannel = it.next()
      if (idle.channel === ch) {
        it.remove()
        done = true
      }
    }
    
    onRemove()
  }
  
  private def onRemove(): Unit = synchronized {
    trace("onRemove()")
    
    val waiting: Promise[Channel] = waitingQueue.poll()
  
    if (null == waiting) {
      count -= 1
      assert(count >= 0, "Invalid Count: "+count)
    }
    else {
      // Replace the removed channel with a new channel
      newChannel(this).onComplete {
        case Success(ch) =>
          ch.closeFuture().onComplete{ case _ => remove(ch) }
          if (!waiting.trySuccess(ch)) release(ch)
        case Failure(ex) => 
          waiting.tryFailure(ex)
          onRemove()
      }
    }
  }
  
  private def trace(msg: => String): Unit = {
    if (logger.isTraceEnabled) logger.trace(s"[$label] - $msg - clientsWaiting: ${waitingQueue.size}, idleChannels: ${idleChannels.size}, activeCount: $count, exceptionCount: $exceptionCount, poolDisabled: $poolDisabled")
  }
  private def error(msg: => String): Unit = {
    if (logger.isErrorEnabled) logger.error(s"[$label] - $msg - clientsWaiting: ${waitingQueue.size}, idleChannels: ${idleChannels.size}, activeCount: $count, exceptionCount: $exceptionCount, poolDisabled: $poolDisabled")
  }

}
