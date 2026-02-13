/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package collection

abstract class MutableMap[Key, Value] extends ReadOnlyMap[Key, Value]:

  def update(k: Key, v: Value): Unit

  def remove(k: Key): Value | Null

  def -=(k: Key): this.type =
    remove(k)
    this

  def clear(resetToInitial: Boolean = true): Unit

  def getOrElseUpdate(key: Key, value: => Value): Value
