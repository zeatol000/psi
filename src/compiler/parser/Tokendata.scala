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
import scala.collection.immutable.BitSet

/**
 * TokenData trait and utility file
 */


type Token = Int
type TokenSet = BitSet
type Offset = Int
def NoOffset: Offset = -1

trait TokenData {
  var token: Token = EMPTY
  var offset: Offset = 0
  var lastOffset: Offset = 0
  var lineOffset: Offset = -1
  var name: Name = null
  var strval: String = null
  var base: Int = 0
  def copyFrom(td: TokenData): Unit = {
    this.token      = td.token
    this.offset     = td.offset
    this.lastOffset = td.lastOffset
    this.lineOffset = td.lineOffset
    this.name       = td.name
    this.strval     = td.strval
    this.base       = td.base
  }
  def isNewLine        = token == NEWLINE
  def isStatSep        = token == NEWLINE || token == SEMI
  def isIdent          = token == IDENTIFIER
  def isIdent(n: Name) = token == IDENTIFIER && name == n
  def isNestStart      = token == LBRACE
  def isNestEnd        = token == RBRACE
  def isAfterLF        = lineOffset >= 0
  def isColon          = token == COLON || token == COLONS
  def isArrow          = token == RARROW
  //def isOperator = token == IDENTIFIER &&
}
