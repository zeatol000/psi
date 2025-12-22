/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

import psi.cc.*

class Reporter
{
  var noWarns: Boolean = false
  var verbose: Boolean = false

  var errors: Int = 0
  var warnings: Int = 0
  var hasCompilerError: Boolean = false
  
  def error(msg: String, file: String, offset: parser.Offset = 0)(using Context): Unit =  // LATER: add better formatting and text highlighting
    errors += 1
    var finalMsg: String = s"[ERR] (_ID_HERE_: ERROR) ${file}:${offset}\n"    // TODO: replace the second ERROR with the name of the error id
    msg.split("\n") foreach { i => finalMsg = finalMsg + s"    | ${i}\n" }
    finalMsg += "    |"

    println(finalMsg)

  def exitCode: Byte = 
    if hasCompilerError then 1
    else if errors > 0 then 2
    else 0

  def vPrint(msg: String): Unit = if (verbose) println(msg)
}
