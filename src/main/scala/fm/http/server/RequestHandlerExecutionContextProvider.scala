package fm.http.server

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, PriorityBlockingQueue, RejectedExecutionException, RejectedExecutionHandler, ThreadFactory, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.ExecutionContext

object RequestHandlerExecutionContextProvider {
  final case class Static(ex: ExecutionContext) extends RequestHandlerExecutionContextProvider {
    def apply(request: Request): ExecutionContext = ex
  }

  /** A helper to create a PriorityThreadPool or a FIFOThreadPool */
  def ThreadPool(threads: Int, queueSize: Int, usePriorityQueue: Boolean): RequestHandlerExecutionContextProvider = {
    if (usePriorityQueue) PriorityThreadPool(threads = threads, queueSize = queueSize)
    else FIFOThreadPool(threads = threads, queueSize = queueSize)
  }

  /**
   * This uses a PriorityBlockingQueue and ThreadPoolExecutor.  The priority for each Runnable is the starting time of
   * the Request such that Requests are processed in FIFO order.  This prevents individual requests from being starved
   * of CPU resources due to having lots of remote calls or Future callbacks since for older requests those callbacks
   * will goto the front of the queue and execute first.  The tradeoff being that queue operations take O(log N) time.
   *
   * @param threads The number of threads
   * @param queueSize The initial (soft) size of our PriorityQueue
   */
  final case class PriorityThreadPool(threads: Int, queueSize: Int) extends RequestHandlerExecutionContextProvider {
    private val executionContext: ExecutionContext = {
      val queue: BlockingQueue[Runnable] = new PriorityBlockingQueue(queueSize, RunnableWithPriority.comparator)

      ExecutionContext.fromExecutorService(new ThreadPoolExecutor(
        threads, // CoreThreads
        threads, // MaxThreads
        60, // KeepAliveTime - I don't think this matters since the core and max threads are the same
        TimeUnit.SECONDS, // KeepAliveTimeUnit - I don't think this matters since the core and max threads are the same
        queue,
        new NamedThreadFactory("HTTP-Handler-PriorityThreadPool"),
        new ExceptionRejectedExecutionHandler()
      ))
    }

    def apply(request: Request): ExecutionContext = {
      new PriorityExecutionContext(executionContext, request.startTimeMillis)
    }
  }

  /**
   * This uses a plain FIFO ArrayBlockingQueue and ThreadPoolExecutor.  This is faster than the PriorityQueue version
   * since Queue operations happen in constant time but individual Requests can be delayed if they have a lot of remote
   * calls or Future callbacks since each callback will goto the end of the queue.
   *
   * @param threads The number of threads
   * @param queueSize The max (hard limit) size of the ArrayBlockingQueue
   */
  final case class FIFOThreadPool(threads: Int, queueSize: Int) extends RequestHandlerExecutionContextProvider {
    private val executionContext: ExecutionContext = {
      val queue: BlockingQueue[Runnable] = new ArrayBlockingQueue(queueSize)

      ExecutionContext.fromExecutorService(new ThreadPoolExecutor(
        threads, // CoreThreads
        threads, // MaxThreads
        60, // KeepAliveTime - I don't think this matters since the core and max threads are the same
        TimeUnit.SECONDS, // KeepAliveTimeUnit - I don't think this matters since the core and max threads are the same
        queue,
        new NamedThreadFactory("HTTP-Handler-FIFOThreadPool"),
        new ExceptionRejectedExecutionHandler()
      ))
    }

    def apply(request: Request): ExecutionContext = executionContext
  }

  final class NamedThreadFactory(name: String) extends ThreadFactory {
    private val threadCount: AtomicInteger = new AtomicInteger(0)
    val group: ThreadGroup = new ThreadGroup(name)
    def newThread(r: Runnable): Thread = {
      val count: Int = threadCount.incrementAndGet
      val t: Thread = new Thread(group, r, name+"-"+count)
      t.setDaemon(true) // Don't prevent JVM shutdown when main exits
      t
    }
  }

  private class ExceptionRejectedExecutionHandler extends RejectedExecutionHandler {
    override def rejectedExecution(r: Runnable, executor: ThreadPoolExecutor): Unit = {
      throw new RejectedExecutionException("Queue full.  Too many requests.")
    }
  }

  /**
   * This simply wraps the Runnable passed into the execute method with a RunnableWithPriority which can be used by a
   * priority queue.
   *
   * @param underlying The actual ExecutionContext to execute on
   * @param priority The priority to pass into RunnableWithPriority
   */
  private class PriorityExecutionContext(underlying: ExecutionContext, priority: Long) extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = underlying.execute(new RunnableWithPriority(runnable, priority))
    override def reportFailure(cause: Throwable): Unit = {}
  }

  private object RunnableWithPriority {
    implicit object comparator extends java.util.Comparator[Runnable] {
      override def compare(a: Runnable, b: Runnable): Int = {
        java.lang.Long.compare(a.asInstanceOf[RunnableWithPriority].priority, b.asInstanceOf[RunnableWithPriority].priority)
      }
    }
  }

  /**
   *
   * @param r The runnable we are wrapping
   * @param priority The priority associated with this runnable
   */
  private class RunnableWithPriority(r: Runnable, val priority: Long) extends Runnable {
    override def run(): Unit = r.run()
  }
}

/**
 * This allows you to provide a custom ExecutionContext based on the Request that gets passed into the
 * RequestHandler.handle method when the HTTP Server is serving a request.  This allows you to do things like provide a
 * PriorityQueue based ThreadPoolExecutor that can use the timestamp from the Request as the priority to ensure that
 * requests are processed in a FIFO (the first request in has the highest priority) ordering to prevent Requests with
 * lots of remote calls or Future callbacks from being starved of CPU time.
 */
trait RequestHandlerExecutionContextProvider {
  /** Given a Request return the Execution to pass into the RequestHandler  */
  def apply(request: Request): ExecutionContext
}
