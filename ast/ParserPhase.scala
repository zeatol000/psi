/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast.parser

import psi.cc.{Phase, Reporter, Context, ctxt}

private[cc]
class Parser
(
  using Context
) extends
    Phase
{
  override def phaseName: String = "Parser"
  override def phaseDesc: String = "Splits files into Array[String] for later semantic analysis"
}
