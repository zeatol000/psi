/*     ____  _____ ____                         *\
*    / __ \/ ___//  _/                         *
*   / /_/ /\__ \ / /       Psi-lang 2025       *
*  / ____/___/ // /                            *
* /_/    /____/___/                            *
\*                                              */

package psi.cc
package parser

import utils.chars.*
import ast.*
import Tokens.*
import utils.*
import scala.collection.mutable
import scala.compiletime.uninitialized

abstract class ScannerCommon
(Content: Seq[Char], Path: String)(using Context)
extends CharSeqReader with TokenData
{
  val buf = Content
  def nextToken(): Unit
  def toToken(name: Name): Token
  def skip(): Unit
  //protected def startLitBufSize: 
  protected var litBuf = mutable.Seq[Char]()
  protected def putChar(c: Char): Unit = litBuf = litBuf :+ c
  protected def clearLit(): Unit = litBuf = mutable.Seq()

  def finishNamed(): Unit = finishNamedTk(IDENTIFIER, this)
  def finishNamedTk(tk: Token, target: TokenData): Unit =
    target.name = Name(litBuf.map(_.toChar).mkString(""))
    litBuf = mutable.Seq()
    target.token = tk
    if tk == IDENTIFIER then
      val conv = toToken(target.name)
      if /*(conv ne END) || */(target eq this) then target.token = conv


  def setStrVal(): Unit =
    strval = litBuf.map(_.toChar).mkString("")
    litBuf = mutable.Seq()

  def checkNoTrailingSeparator(): Unit =
    if !litBuf.isEmpty then
      error(er"Trailing separator is not allowed", offset + litBuf.length - 1)
}



abstract class CharSeqReader(using Context) { self =>

  val buf: Seq[Char]
  protected def startFrom: Int = 0

  protected def decodeUni: Boolean = false

  var ch: Char = uninitialized
  var charOffset: Int = startFrom
  var lastCharOffset: Int = startFrom
  var lineStartOffset: Int = startFrom
  private var lastUnicodeOffset = -1

  def error(msg: String, offset: Int): Unit

  def isUnicodeEscape: Boolean = charOffset == lastUnicodeOffset

  final def nextChar(): Unit = {
    val idx = charOffset
    lastCharOffset = idx
    charOffset = idx + 1
    if idx >= buf.length then
      ch = SU
    else
      val c = buf(idx)
      ch = c
      if c == '\\' then potentialUnicode()
      else if c < ' ' then { skipCR(); potentialLineEnd() }
  }

  def getc(): Char = { nextChar(); ch }

  final def nextRawChar(): Unit = {
    val idx = charOffset
    lastCharOffset = idx
    charOffset = idx + 1
    if idx >= buf.length then
      ch = SU
    else
      val c = buf(idx)
      ch = c
      if c=='\\'then potentialUnicode()
  }

  private def potentialUnicode(): Unit = {
    def evenSlashPrefix: Boolean = {
      var p = charOffset - 2
      while (p >= 0 && buf(p) == '\\') p -= 1
      (charOffset - p) % 2 == 0
    }
    def udigit: Int =
      if (charOffset >= buf.length) {
        error(er"incomplete unicode escape", charOffset - 1)
        SU
      }
      else {
        val d = digit2int(buf(charOffset), 16)
        if (d >= 0) charOffset += 1
        else error(er"error in unicode escape", charOffset)
        d
      }
    if (charOffset < buf.length && buf(charOffset) == 'u' && decodeUni && evenSlashPrefix) {
      while ({
        charOffset += 1
        charOffset < buf.length && buf(charOffset) == 'u'
      })
      ()
      val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
      lastUnicodeOffset = charOffset
      ch = code.toChar
    }
  }

  private def skipCR(): Unit =
    if ch == CR then
      if charOffset < buf.length && buf(charOffset) == LF then
        charOffset += 1
        ch = LF

  private def potentialLineEnd(): Unit =
    if ch == LF || ch == FF then lineStartOffset = charOffset

  def isAtEnd: Boolean = charOffset >= buf.length

  def lookaheadReader(): CharSeqLookaheadReader = new CharSeqLookaheadReader

  def lookaheadChar(): Char = lookaheadReader().getc()

  class CharSeqLookaheadReader extends CharSeqReader {
    val buf: Seq[Char] = self.buf
    charOffset = self.charOffset
    ch = self.ch
    override def decodeUni: Boolean = self.decodeUni
    def error(msg: String, offset: Int): Unit = self.error(msg, offset)
  }
}
