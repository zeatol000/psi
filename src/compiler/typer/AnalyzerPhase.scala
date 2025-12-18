/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package typer

import psi.cc.*

private[cc]
class AnalyzerPhase
extends
  Phase
{
  def phaseName: String = "Analyzer"
  def phaseDesc: String = "Assign types to definitions and enforce syntax"
  def run(using Context): Unit =
    val analyzers: Array[Analyzer] = new Array(ctxt.args.psiFiles.length)

    for (i <- 0 until analyzers.length)
      analyzers(i) = new Analyzer(ctxt.args.psiFiles(i))
      analyzers(i).temp // temp
}
