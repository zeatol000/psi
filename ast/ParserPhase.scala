/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import psi.cc.*

private[cc]
class Parser
extends
    Phase
{
  def phaseName: String = "Parser"
  def phaseDesc: String = "Scan and parse sources"
  def run(using Context): Unit =
    val parsers: Array[FileParser] = new Array(ctxt.args.psiFiles.length)

    for (i <- 0 until parsers.length)
      parsers(i) = new FileParser(ctxt.args.psiFiles(i))
      parsers(i).parse
}
