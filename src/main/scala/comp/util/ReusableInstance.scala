/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import scala.collection.mutable.ArrayBuffer
import psic.tools.comp.util.Chaining.*

object ReusableInstance {
  private inline val InitialSize = 4
  def apply[T <: AnyRef](make: => T): ReusableInstance[T] = new ReusableInstance[T](make)
}

final class ReusableInstance[T <: AnyRef] private (make: => T) {
  private val cache = new ArrayBuffer[T](ReusableInstance.InitialSize).tap(_.addOne(make))
  private var taken = 0

  inline def withInstance[R](op: T => R): R =
    if taken == cache.size then cache += make
    taken += 1
    try op(cache(taken - 1)) finally taken -= 1
}
