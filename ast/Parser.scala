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
import scala.collection.mutable
//import scala.jdk.CollectionConverters.*
//import java.nio.file.{Files, Paths, Path}
//import java.io.FileInputStream


private[ast]
class FileParser
(
  val path: String,
)
{
  inline val CR = 0x000D // Carriage Return -- \r
  inline val LF = 0x000A // Line Feed -- \n
  
  var sc: Scanner = null  /* Takes the Seq[Byte] and provides utilities to read
                             and tokenize the Seq[Byte] */
  var ast: AST = null     /* Provides a LOT */
  var pn: PatternN = null /* Tansforms people's weird syntax into normal syntax */

  var loopDepth: Byte = 0 // what kind of instane bastard would use 128 nested blocks

  def parse(using Context): Unit =
    vp"parsing $path"
    ast = ctxt.ast
    sc = new Scanner( File(path).read, path )

    while (sc.token ne EOF) {
      val stack = sc.stack
      val token = sc.token
      sc.fetchToken()
      (sc.token eq IDENTIFIER, token eq IDENTIFIER) match {
        //case (false, true)  if isKeyword(sc.token)  => P(token, sc.stack)
        case (false, true)  if !isKeyword(sc.token) => P(token, stack)
        case (false, false)                         => (sc.token, token) match {
          case (_, EMPTY)                           => void()
          case (EMPTY, STRINGLIT) | (EMPTY, CHARLIT)  => P(token, stack)
          case (CHARLIT, CHARLIT) | (STRINGLIT, STRINGLIT) => void()
          case (_, INTLIT) if sc.token ne INTLIT    => P(token, stack)
          case (INTLIT, INTLIT)                     => void()
          case (_, _)                               => P(token, stack)
        }
        case (true, false)                          => P(token, stack)
        case (true, true) if ((stack.length eq sc.stack.length)) => P(token, stack)
        case (_, _) => void()
      }
    }

    // TODO:
    // - sc: be able to tokenize things
    // - this: read the entire file and use the scanner
    // - pn: transform into the normal patterns
    // - ast: push the syntax made by this into an internal untpd class,
    //   write it to a file, and keep track of all untpd files.
  
  inline def P(t: Int, s: mutable.Seq[Byte]): Unit =
    println(s"(${t}) ${tokenStr(t)} | ${s.map(_.toChar).mkString("")}")
  inline def void(): Unit = () 

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
    (sc.token == MOD)   )
  def isDeclare: Boolean = (
    (sc.token == VAL) ||
    (sc.token == VAR) ||
    (sc.token == PRO) ||
    (sc.token == SUB) ||
    (sc.token == CO)  ||
    (sc.token == FN)  ||
    (sc.token == TYPE))
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
