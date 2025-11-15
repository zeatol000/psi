/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

import psi.cc.{Phase, Reporter, Context, ctxt}

private[cc]
class Initial(
  using Context
) extends
    Phase
{
  override def phaseName: String = "Initial"
  override def phaseDesc: String = "Gets the environment ready for compilation"
}
