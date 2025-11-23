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
import psi.cc.*

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

  def vp(args: Any*)(using Context): Unit =
    val format = sc.s(args*)
    ctxt.reporter.vPrint(format)

  def er(args: Any*)(using Context): String =
    // TODO: error formatting
    sc.s(args*)
