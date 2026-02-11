/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import scala.annotation.tailrec
import scala.reflect.ClassTag
import psic.tools.collection.SixteenNibbles

object LRUCache {
  val Retained: Int = 16
  val initialRing: SixteenNibbles =
    (0 until Retained).foldLeft(new SixteenNibbles(0L)){(n, idx) =>
      n.updated(idx, (idx + 1) % Retained)
    }
}

class LRUCache [K >: Null <: AnyRef | Null: ClassTag, V >: Null: ClassTag] {
  import LRUCache.*

  val keys   = new Array[K](Retained)
  val values = new Array[V](Retained)
  var next   = new SixteenNibbles(initialRing.bits)
  var last: Int = Retained - 1
  var lastButOne: Int = last - 1

  def first: Int = next(last)

  def lookup(key: K): V = {
    @tailrec def Next(prev: Int, current: Int, nx: SixteenNibbles): V =
      val follow = nx(current)
      if (keys(current) == key) {
        if      (current == last) last = prev
        else if (prev != last)    next = next
          .updated(prev, follow  )
          .updated(current, first)
          .updated(last, current )
        values(current)
      }
      else if (current == last) {
        lastButOne = prev
        null
      }
      else Next(current, follow, nx)
    end Next

    Next(last, first, next)
  }

  def enter(key: K, value: V): Unit =
    keys(last) = key
    values(last) = value
    last = lastButOne

  def invalidate(key: K): Unit =
    if lookup(key) != null then
      keys(first) = null
      last = first

  def indicies: Iterator[Int] = Iterator.iterate(first)(next.apply)

  def keysIterator: Iterator[K] =
    indicies take Retained map keys filter (_ != null)

  override def toString: String =
    s"LRUCache(${keysIterator
      .toList
      .reverse
      .map(key => s"$key -> ${lookup(key)}")
      .reverse
      .mkString(", ")
    })"
}
