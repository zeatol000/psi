/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              *

package psic.tools
package collection

abstract class MutableSet[T] extends ReadOnlySet[T]:

  def +=(x: T): Unit

  def add(x: T): Boolean =
    if lookup(x) == null then { this += x; true }
    else false

  def put(x: T): T

  def -=(x: T): Unit

  def clear(resetToInitial: Boolean = true): Unit

  def ++= (xs: IterableOnce[T]): Unit =
    xs.iterator.foreach(this += _)

  def --= (xs: IterableOnce[T]): Unit =
    xs.iterator.foreach(this -= _)
*/
