/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import psi.cc.*
import psi.cc.utils.*
import psi.cc.ast.*
import psi.cc.ast.Tokens.*
import scala.collection.immutable.BitSet

private[ast]
class FileParser(file: String)
{
  val sc: Scanner = null
  val ast: AST = null
  val pn: PatternN = null

  var loopDepth: Byte = 0 // what kind of instane bastard would use 128 nested blocks

  def parse(using Context): Unit =
    vp"parsing $file"

    // instantiate scanner, ast, and pattern normalizer
    // push ast to Context

  
  // errors ///////////////////////////////////////////////////////////////////
  // classifying tokens ///////////////////////////////////////////////////////
  def isModifier: Boolean = (
    (sc.token == ABSTRACT)  ||
    (sc.token == FINAL)     ||
    (sc.token == PRIVATE)   ||
    (sc.token == PROTECTED) ||
    (sc.token == OVERRIDE)  )
  def isDefine: Boolean = (
    (sc.token == VAL)   ||
    (sc.token == VAR)   ||
    (sc.token == PRO)   ||
    (sc.token == SUB)   ||
    (sc.token == CO)    ||
    (sc.token == FN)    ||
    (sc.token == OBJ)   ||
    (sc.token == CLASS) ||
    (sc.token == TRAIT) ||
    (sc.token == TYPE)  ||
    (sc.token == MOD)     )
  def isDeclare: Boolean = (
    (sc.token == VAL) ||
    (sc.token == VAR) ||
    (sc.token == PRO) ||
    (sc.token == SUB) ||
    (sc.token == CO)  ||
    (sc.token == FN)  ||
    (sc.token == TYPE)     )
  def isExpressionStart: Boolean = (
    (sc.token == IDENTIFIER)  ||
    (sc.token == CHARLIT)     ||
    (sc.token == INTLIT)      ||
    (sc.token == LONGLIT)     ||
    (sc.token == FLOATLIT)    ||
    (sc.token == DOUBLELIT)   ||
    (sc.token == STRINGLIT)   ||
    (sc.token == NULL)        ||
    (sc.token == TRUE)        ||
    (sc.token == FALSE)       ||
    (sc.token == THIS)        ||
    (sc.token == SUPER)       ||
    (sc.token == NEW)         ||
    (sc.token == IF)          ||
    (sc.token == FOR)         ||
    (sc.token == TRY)         ||
    (sc.token == WHILE)       ||
    (sc.token == RETURN)      ||
    (sc.token == THROW)       ||
    (sc.token == LPAREN)      ||
    (sc.token == LBRACE)      )
}
