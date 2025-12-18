/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

/**
 * Format Fatal Errors and similar.
 * Add more error formatters if needed
 */

import psi.cc.*

def Error(msg: String, path: String)(using Context): Unit =
  ctxt.reporter.error(msg, path)

def FatalError(phase: String, exception: Exception): Unit =
  println(s"""
    |Fatal error during compilation..
    |
    |Error details:
    |   Psi Version -> $PsiVersion
    |   During Phase -> $phase
    |   Exception Message -> ${exception.getMessage()}
    |
    |Printing stack trace...
    """.stripMargin
  ); exception.printStackTrace()
  System.exit(1)

def FatalError(phase: Phase, exception: Exception): Unit = 
  println(s"""
    |Fatal error during compilation..
    |
    |Error details:
    |   Psi Version -> $PsiVersion
    |   During Phase -> ${phase.phaseName}
    |   Phase Description -> ${phase.phaseDesc}
    |   Exception Message -> ${exception.getMessage()}
    |
    |Printing stack trace...
    """.stripMargin
  ); exception.printStackTrace()
  System.exit(1)

def NoStackError(msg: String)(using phase: Phase): Unit =
  NoStackError(phase, msg)

def NoStackError(phase: Phase, msg: String): Unit =
  println(s"""
    |Fatal error during compilation..
    |
    |Error details:
    |   Psi Version -> $PsiVersion
    |   During Phase -> ${phase.phaseName}
    |   Phase Description -> ${phase.phaseDesc}
    |   Exception Message ->  ${msg}
    """.stripMargin)
  System.exit(2)

def ExternalError(msg: String): Unit =
  println(s"""
    |$msg
    |Use "help" for help
    """.stripMargin)
  System.exit(2)
