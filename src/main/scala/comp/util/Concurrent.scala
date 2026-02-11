/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Failure}

object Concurrent
{

  class NoCompletion extends RuntimeException


  class Future[T](exec: Executor[T]) {
    private var res: Option[Try[T]] = None

    def force: Try[T] = synchronized {
      while (res.isEmpty && exec.isAlive) wait(1000)
      res.getOrElse(Failure(NoCompletion()))
    }

    def complete(r: Try[T]): Unit = synchronized {
      res = Some(r)
      notifyAll()
    }
  }


  class Executor[T] extends Thread {
    private type Job = (Future[T], () => T)
    private var allScheduled = false
    private val pending = new ArrayBuffer[Job]

    def schedule(fn: () => T): Future[T] = synchronized {
      assert(!allScheduled)
      val f = Future[T](this)
      pending += ((f, fn))
      notifyAll()
      f
    }

    def close(): Unit = synchronized {
      allScheduled = true
      notifyAll()
    }

    private def nextPending(): Option[Job] = synchronized {
      while (pending.isEmpty && !allScheduled) wait(1000)
      if (pending.isEmpty) None
      else
        pending.dropInPlace(1)
        Some(pending.head)
    }

    override def run(): Unit =
      while
        nextPending() match
          case Some((f, op)) =>
            f.complete(Try(op()))
            true
          case None =>
            false
      do ()
  }
}
