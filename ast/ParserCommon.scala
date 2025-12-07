/*     ____  _____ ____                         *\
*    / __ \/ ___//  _/                         *
*   / /_/ /\__ \ / /       Psi-lang 2025       *
*  / ____/___/ // /                            *
* /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import utils.chars.*
import ast.*
import Tokens.*
import utils.*
import scala.collection.mutable
import scala.compiletime.uninitialized

//case class OpInfo(operand: AST, operator: , offset: Offset)

enum Location (val inParens: Boolean, val inPattern: Boolean, val inArgs: Boolean)
{ // I HAVE NO IDEA HOW TO USE ENUMS!!!! SCALA'S DOCUMENTATION SUCKS!!!! I LOVE SCALA
  case InParens      extends Location(true,  false, false)
  case InArgs        extends Location(true,  false, true )
  case InColonArg    extends Location(false, false, true )
  case InPattern     extends Location(false, true,  false)
  case InGuard       extends Location(false, false, false)
  case InPatternArgs extends Location(false, true,  true )
  case InBlock       extends Location(false, false, false)
  case ElseWhere     extends Location(false, false, false)
}

enum ParamOwner
{
  case Class    // class or trait
  case Fn       // functions and concurrents
  case Op       // operations
  case Type     // type alias
  case Hk       // type parameter

  def isClass = this == Class
  def acceptsWildcard = this == Type || this == Hk
}

enum ParseKind
{
  case Expr, Type, Pattern
}

type StageKind = Int
object StageKind
{
  inline val None          = 0
  inline val Quoted        = 1
  inline val Spliced       = 1 << 1
  inline val QuotedPattern = 1 << 2
}

//private[ast] val InCase: Region => Region = InCase(_)
//private[ast] val InCond: Region => Region = InParens


abstract class ParserCommon
(Content: Seq[Char], Path: String)(using Context)
{
  val sc: ScannerCommon
  val ast: AST
  protected def skip(): Unit
  protected var lastErrorOffset: Offset = -1
  def syntaxError(msg: String, off: Offset, skip: Boolean): Unit =
    if off > lastErrorOffset then
      sc.error(msg, off)
      lastErrorOffset = off
    if skip then sc.skip()
 }

/*trait OutlineParserCommon extends ParserCommon
{
  def accept(token: Token): Token
  def skipBracesHook(): 
}*/
