/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import psi.cc.*
import utils.*

private[ast]
class Parser(Path: String)(using Context)
{
  val ast: AST    = ctxt.ast
  val Content: Seq[Char] = File(Path).read
  val sc: Scanner = new Scanner( Content, Path )

  def parse: Unit = {

    if Content.isEmpty then Error(er"Content returned null", Path)

    Content.foreach(print)
  }
}
