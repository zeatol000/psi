/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

import psi.cc.{Context, ctxt}
import psi.cc.ast.parser.Parser

private[cc]
def CompilerPhases(): List[Phase] =
  new Initial()   ::  // Find all options and included files including jars
  new Parser()    ::  // Tokenize and parse source files
  //new Analyzer(c)  ::  // Name and type checking
  //new BCodeGen(c)  ::  // Class file generation
  //new Packager(c)  ::  // Package into a jar if told
  Nil


private[cc]
class Phase
{
  def phaseName: String
  def phaseDesc: String 
  def isEnabled(using Context): Boolean = true

  
  def run(using ctxt: Context): Unit

}
