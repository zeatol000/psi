/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import scala.util.boundary

type Optional[T] = boundary.Label[None.type] ?=> T

object Optional {
  inline def apply[T](inline body: Optional[T]): Option[T] = boundary(Some(body))

  inline def break()(using boundary.Label[None.type]): Nothing = boundary.break(None)

  extension [T](r: Option[T])
    inline def ? (using boundary.Label[None.type]): T = r match
      case Some(x) => x
      case None => boundary.break(None)
}
