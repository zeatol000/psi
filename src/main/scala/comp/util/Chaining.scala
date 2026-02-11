/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

object Chaining {
  extension[A] (x: A)
    inline def tap(inline f: A => Unit ): A = {  f(x); x  }
    inline def pipe[B](inline f: A => B): B = f(x)
}
