/*     ____  _____ ____                         *\
*    / __ \/ ___//  _/                         *
*   / /_/ /\__ \ / /       Psi-lang 2025       *
*  / ____/___/ // /                            *
* /_/    /____/___/                            *
\*                                              */

package psi.cc
package parser

import ast.*
import Tokens.*
import utils.*
import scala.annotation.tailrec

/** A region is what closes a token that takes a block of data
 *  A region can be one of the following:
 *
 *  string interpolation
 *  parentheses (including brackets, sharps)
 *  braces
 *  case (in a match)
 *
 *  This probably needs improvement
 */

case class InString(outer: Region) extends Region(RBRACE)
case class InParens(prefix: Token, outer: Region) extends Region(prefix + 1)
case class InBraces(outer: Region) extends Region(RBRACE)
//case class InCase(outer: Region) extends Region(CASE)
case class InTopLevel(outer: Region) extends Region(EOF)

abstract class Region(val closedBy: Token)
{
  private var _ce: Boolean = false
  def outer: Region | Null
  def isOuterMost = outer == null
  def enclosing: Region = outer.asInstanceOf[Region]


  inline def commasExpected = _ce
  inline def withCommasExpected[T](inline fn: => T): T =
    val saved = _ce
    _ce = true
    val res = fn
    _ce = false
    res



  def toList: List[Region] = 
    this :: (if outer == null then Nil else outer.toList)


  private inline def delimiter = this match {
    case _: InString => "}"
    case InParens(LPAREN, _) => ")"
    case InParens(LBRACKET, _) => "]"
    case InParens(LSHARP, _) => ">"
    case _: InBraces => "}"
    //case _: inCase => 
  }
}
