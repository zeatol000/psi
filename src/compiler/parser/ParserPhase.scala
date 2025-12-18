/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package parser

import psi.cc.*
import ast.*
import utils.File

private[cc]
class ParserPhase
extends
    Phase
{
  def phaseName: String = "Parser"
  def phaseDesc: String = "Scan and parse sources"
  def run(using Context): Unit =
    val parsers: Array[Parser] = new Array(ctxt.args.psiFiles.length)

    ctxt ++ new AST
    ctxt.ast ++ new NameTable

    for (i <- 0 until parsers.length)
      parsers(i) = new Parser(
        File(ctxt.args.psiFiles(i)).read,
        ctxt.args.psiFiles(i)
      )
      parsers(i).parse
}
