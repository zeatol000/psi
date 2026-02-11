/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp
package object util {

  def bestFit(candidates: Array[Int], length: Int, x: Int, hint: Int = -1): Int = {
    def f(lo: Int, hi: Int, mi: Int): Int =
      if      x < candidates(mi)                         then f(lo, mi - 1, (lo + mi - 1) / 2)
      else if mi + 1 < length && x >= candidates(mi + 1) then f(mi + 1, hi, (mi + hi + 1) / 2)
      else mi

    val mid = if (0 <= hint && hint < length) hint else length / 2
    if (length == 0 || x < candidates(0)) -1
    else f(0, length, mid)
  }

}
