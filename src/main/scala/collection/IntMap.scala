/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package collection

import psic.tools.comp.util.PerfectHashing

import scala.compiletime.uninitialized

final class IntMap[K](initialCapacity: Int = 8, capacityMultiple: Int = 2) extends PerfectHashing[K](initialCapacity, capacityMultiple) {
  private var values: Array[Int] = uninitialized
  
  def default: Int = -1
  
  protected override def allocate(capacity: Int) =
    super.allocate(capacity)
    values = new Array[Int](capacity)
  
  def apply(k: K): Int =
    val i = index(k)
    if (i < 0) default else values(i)

  def update(k: K, v: Int): Unit =
    val i = add(k)
    values(i) = v

  protected override def growTable() =
    val old = values
    super.growTable()
    Array.copy(old, 0, values, 0, old.length)

  def valuesIterator = values.iterator.take(size)

  def iterator: Iterator[(K, Int)] = keysIterator.zip(valuesIterator)

  def value(i: Int)  = values(i)

  def setValue(i: Int, v: Int) = values(i) = v

  override def toString = (iterator.map: (k, v) =>
      s"$k -> $v").mkString("IntMap(", ", ", ")")
}
