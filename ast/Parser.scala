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
import ast.*

private[ast]
class Parser
(Content: Seq[Char], Path: String)(using Context)
extends ParserCommon(Content, Path)
{
  val ast: AST    = ctxt.ast
  val sc: Scanner = new Scanner( Content, Path )
  protected def skip(): Unit = ()

  def parse: Unit = {

    if Content.isEmpty then
      Error(er"File content returned null\nCannot continue", Path)
      return

    Content.foreach(print)
  }
}
