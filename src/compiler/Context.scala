/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

/**
 * Context is all about giving phases the info they need.
 * Such as what files it needs to compile,
 * where the .ast and .tast files are
 * 
 * The implicit stuff is basically an auto-parameter pushing
 * data barrel
 */

import psi.cc.*
import psi.cc.ast.*

// Easy access
inline def ctxt(using c: Context): Context = c
inline def vPrint(x: String)(using c: Context): Unit = c.reporter.vPrint(x)

private[cc]
abstract class Context
{
  def reporter: Reporter
  def args: cliArgs
  //def ast: AST
  //def ++(x: AST): Unit

  protected given Context = this
}

class ContextInit
(
  var _reporter: Reporter,
  var _args: cliArgs,
  //var _ast: AST = null,
) extends
    Context
{
  def reporter: Reporter = _reporter
  def args: cliArgs = _args
  //def ast: AST = _ast
  //def ++(x: AST): Unit = _ast = x
}
