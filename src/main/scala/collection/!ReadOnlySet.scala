/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              *

package psic.tools
package collection

abstract class ReadOnlySet[T]:

  def lookup(x: T): T | Null

  def size: Int

  def iterator: Iterator[T]

  def contains(x: T): Boolean = lookup(x) != null

  def foreach[U](f: T => U): Unit = iterator.foreach(f)

  def toList: List[T] = iterator.toList

  def isEmpty = size == 0

object ReadOnlySet:
  def empty[T]: ReadOnlySet[T] = HashSet[T](4)
*/
