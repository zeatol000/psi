/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

object Common {
  val alwaysTrue:  Any => Boolean = Function.const(true)
  val alwaysFalse: Any => Boolean = Function.const(false)
  val alwaysZero:  Any => Int     = Function.const(0)
}
