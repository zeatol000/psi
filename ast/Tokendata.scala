/*     ____  _____ ____                         *\
*    / __ \/ ___//  _/                         *
*   / /_/ /\__ \ / /       Psi-lang 2025       *
*  / ____/___/ // /                            *
* /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import ast.*
import Tokens.*
import utils.*
import scala.collection.immutable.BitSet

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
  def cp(td: TokenData): Unit = {
    this.token      = td.token
    this.offset     = td.offset
    this.lastOffset = td.lastOffset
    this.lineOffset = td.lineOffset
    this.name       = td.name
    this.strval     = td.strval
  }
  def isStatEnd = token == SEMI
  def isIdent   = token == IDENTIFIER
  def isIdent(n: Name) = token == IDENTIFIER && name == n
  def isBStart  = token == LBRACE
  def isBEnd    = token == RBRACE
}
