/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

import psi.cc.*

/**
 * Context is all about giving phases the info they need.
 * Such as what files it needs to compile,
 * where the .ast and .tast files are
 * 
 * i have no clue how to use givens LMAO
 */

inline def ctxt(using c: Context): Context = c

private[cc]
abstract class Context
{
  def reporter: Reporter
  def args: cliArgs

  protected given Context = this
}

class ContextInit
(
  var _reporter: Reporter,
  var _args: cliArgs
) extends
    Context
{
  def reporter: Reporter = _reporter
  def args: cliArgs = _args
}
