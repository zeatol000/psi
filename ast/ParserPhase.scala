/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast.parser

import psi.cc.*

private[cc]
class Parser
(
  using Context
) extends
    Phase
{
  def phaseName: String = "Parser"
  def phaseDesc: String = "Splits files into Array[String] for later semantic analysis"
  def run(using Context): Unit =
    println(ctxt.args.dump)
    // TODO: implement lexer and untyped parser
}
