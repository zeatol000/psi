/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package Manual

import psi.cc.PsiVersion
import scala.io.AnsiColor.*

def printHelp(x: Int): Unit =
  println( x match {
    case 1 => defaultHelp

  }); System.exit(2)


// would be nice to have actual documentation. and maybe even a link to it
def defaultHelp: String = s"""${RESET}Psi - $PsiVersion

${UNDERLINED}Help overview -- please see actual documentation$RESET

Usage:$BOLD psi <args> $RESET

ags can be main commands, flags, files, or --
all args after -- are pushed to the target program

Commands:
$BOLD   run       $RESET   Compile and run a target project
$BOLD   compile   $RESET   Compile a project to .class files
$BOLD   package   $RESET   Compile a project to .jar files (including libraries)
$BOLD   clean     $RESET   Remove binaries (usually .class files)
$BOLD   repl      $RESET   Launch REPL
$BOLD   help      $RESET   Show this help section
"""
