/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

import psi.cc.*
import psi.cc.ast.Parser
import psi.cc.utils.FatalError

private[cc]
def CompilerPhases(using Context): List[Phase] =
  new Initial()   ::  // Find all options and included files including jars
  new Parser()    ::  // Tokenize and parse source files
  //new Analyzer()  ::  // Name and type checking
  //new BCodeGen()  ::  // Class file generation
  //new Packager()  ::  // Package into a jar if told
  Nil


private[cc]
abstract
class Phase
{
  def phaseName: String
  def phaseDesc: String 
  def isEnabled(using Context): Boolean = true

  def _run(using Context): Unit =
    if ctxt.reporter.hasCompilerError then return ()
    try { run }
    catch {
      case e: Exception =>
        FatalError(this, e)
    }

  def run(using Context): Unit

}
