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
import scala.collection.mutable

abstract class ScannerCommon
(Content: Seq[Byte], Path: String)(using Context)
extends TokenData
{
  val buf = Content
  def nextToken(): Unit
  def toToken(name: Name): Token
  //protected def startLitBufSize: 
  protected var litBuf = mutable.Seq[Byte]()
  protected def pushChar(c: Byte): Unit = litBuf = litBuf :+ c

  def finishIdent(): Unit = finishNamedTk(IDENTIFIER, this)
  def finishNamedTk(tk: Token, target: TokenData): Unit =
    target.name = Name(litBuf.map(_.toChar).mkString(""))
    litBuf = mutable.Seq()
    target.token = tk
    if tk == IDENTIFIER then
      val conv = toToken(target.name)
      if /*(conv ne END) || */(target eq this) then target.token = conv


  def setStringVal(): Unit =
    strval = litBuf.map(_.toChar).mkString("")
    litBuf = mutable.Seq()
}

