/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

/**
 * String interpolators
 */

import scala.collection.mutable.ArrayBuffer

extension (sc: StringContext)
  def frm(args: Any*): String =
    val formatted = args.map { x => x match {

      case xa: ArrayBuffer[?] =>
        if xa.length eq 1 then xa(0)
        else if xa.length eq 0 then "\\None\\"
        else xa.mkString(", ")

      case xa: Array[?] =>
        if xa.length eq 1 then xa(0)
        else if xa.length eq 0 then "\\None\\"
        else xa.mkString(" ")

      case _ => x
    }}.toSeq
    sc.s(formatted*)
