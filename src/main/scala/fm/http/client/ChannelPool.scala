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
import java.io.Closeable
import java.util.{Deque, Queue}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedDeque, LinkedBlockingQueue}
import java.util.concurrent.atomic.AtomicInteger
import io.netty.channel.Channel
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object ChannelPool {
  private case class IdleChannel(channel: Channel, lastActivity: Long)
}

final case class ChannelPool(label: String, newChannel: ChannelPool => Future[Channel], limit: Int, maxIdleMillis: Long)(implicit executionCtx: ExecutionContext) extends Logging {
  import ChannelPool.IdleChannel
  
  private[this] var count: Int = 0
  private[this] val waitingQueue: Queue[Promise[Channel]] = new LinkedBlockingQueue(1024 /* TODO: make this configurable */)
  private[this] val idleChannels: Deque[IdleChannel] = new ConcurrentLinkedDeque()

  def closeIdleChannels(): Unit = {
    if (idleChannels.isEmpty()) return
    
    synchronized { 
      trace("closeIdleChannels()")
      
      val oldestAge: Long = System.currentTimeMillis() - maxIdleMillis
      
      val it = idleChannels.iterator()
      
      while(it.hasNext) {
        val idle: IdleChannel = it.next()
        if (idle.lastActivity < oldestAge) {
          idle.channel.close()
          it.remove()
        }
      }
    }
  }
  
  def checkout(): Future[Channel] = synchronized {
    trace("checkout()")
    
    val idle: IdleChannel = idleChannels.poll()
    
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
        case Failure(ex) => onRemove()
      }
      
      res
    } else {
      trace(s"checkout() - Queueing")
      val p: Promise[Channel] = Promise()
      if (!waitingQueue.offer(p)) {
        val msg = s"ChannelPool ($label) waitingQueue is full!"
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
    
    var done = false
    val it = idleChannels.iterator()
    
    while(it.hasNext() && !done) {
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
      require(count >= 0, "Invalid Count: "+count)
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
    if (logger.isTraceEnabled) logger.trace(s"[$label] - $msg - clientsWaiting: ${waitingQueue.size}, idleChannels: ${idleChannels.size}, activeCount: $count")
  }
}
