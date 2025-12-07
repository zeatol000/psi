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
import scala.annotation.{tailrec, switch}
import java.util.Objects

class Scanner
(Content: Seq[Char], Path: String)(using Context)
extends ScannerCommon(Content, Path)
{

  var ctxtArgDeclares: Boolean = true          // >--arg things
  var currentRegion: Region = InTopLevel(null) // active region -- top level (outer region = null)

  def toToken(id: Name): Token = IDENTIFIER
  def newTD: TokenData = new TokenData {}

  val next = newTD
  private val prev = newTD

  private var skipping = false

  def error(msg: String, off: Offset): Unit =
    token = ERROR
    ctxt.reporter.error(msg, Path, off)

  
  private def inInterpolation = currentRegion match {
    case InBraces(InString(_)) => true
    case _ => false
  }

  def skipToken: Offset =
    val o = offset
    nextToken()
    o

  private inline def dropUntil(inline matches: Region => Boolean): Unit =
    while (!matches(currentRegion) && !currentRegion.isOuterMost)
      currentRegion = currentRegion.enclosing

  def adjustSepRegions(last: Token): Unit = (last: @switch) match {
    case LPAREN | LBRACKET | LSHARP =>
      currentRegion = InParens(last, currentRegion)
    case LBRACE =>
      currentRegion = InBraces(currentRegion)
    case RBRACE =>
      dropUntil(_.isInstanceOf[InBraces])
      if (!currentRegion.isOuterMost) currentRegion = currentRegion.enclosing
    case RPAREN | RBRACKET | RSHARP =>
      currentRegion match {
        case InParens(p, o) if p + 1 == last => currentRegion = o
        case _ => }
    case STRINGLIT =>
      currentRegion match {
        case InString(o) => currentRegion = o
        case _ => }
    case _ => }

  private def getNextTk(last: Token): Unit =
    if next.token != EMPTY then
      this.copyFrom(next)
      next.token = EMPTY
    else
      lastOffset = lastCharOffset
      currentRegion match {
        case InString(_) => fetchStringPart()
        case _ => fetchToken()
      }
      if token == ERROR then adjustSepRegions(STRINGLIT)

  def nextToken(): Unit =
    val lastTk = token
    val lastName = name
    adjustSepRegions(lastTk)
    getNextTk(lastTk)
    //if isAfterLF then handleLF(lastTk)
    postProcessToken(lastTk, lastName)
    //printState()

  //def printState(): Unit =
    //if debugStream 

  def insert(tk: Token, offset: Offset) = {
    assert(next.token == EMPTY, next)
    next.copyFrom(this)
    this.offset = offset
    this.token = tk
  }

  //def handleLF(last: Token): Unit =

  def peekAhead() =
    prev.copyFrom(this)
    getNextTk(token)
  
  def reset() =
    next.copyFrom(this)
    this.copyFrom(prev)
  
  def skip(): Unit = {
    val lastReg = currentRegion
    skipping = true
    def atStop =
      token == EOF
      || (currentRegion eq lastReg)
          && (isStatEnd
              || closingParens.contains(token) && lastReg.toList.exists(_.closedBy == token)
              || token == COMMA && lastReg.toList.exists(_.commasExpected)
            )
    end atStop
    var noProg = 0
    var prevOff = offset
    while !atStop && noProg < 3 do
      nextToken()
      if offset <= prevOff then noProg += 1
      else
        prevOff = offset
        noProg = 0
    skipping = false
  }

  def postProcessToken(lasttoken: Token, lastname: Name): Unit =
    def fuse(tk: Token) =
      token = tk
      offset = prev.offset
      lastOffset = prev.lastOffset
      lineOffset = prev.lineOffset
    (token: @switch) match
      case SEMI =>
        peekAhead()
        if token != ELSE then reset()
      case COMMA =>
        def isInParens(r: Region): Boolean = r match
          case _: InParens => true
          case _ => false
        peekAhead()
        if isAfterLF
          && currentRegion.commasExpected
          && (token == RPAREN || token == RBRACKET || token == RSHARP || token == RBRACE)
        then
          lastOffset = prev.lastOffset
        else
          reset()
      case COLON =>
        peekAhead()
        if token == COLON then
          token == COLONS
        else
          reset()
      //case RPAREN | RBRACKET | RSHARP | RBRACE | EOF =>
      case _ =>


  def fetchToken(): Unit = {
    offset = charOffset - 1
    lineOffset = if lastOffset < lineStartOffset then lineStartOffset else -1
    name = null
    ch match {
      case ' ' | '\t' | CR | LF =>
        nextChar()
        fetchToken()
      case 'A' | 'B' | 'C' | 'D' | 'E'
         | 'F' | 'G' | 'H' | 'I' | 'J'
         | 'K' | 'L' | 'M' | 'N' | 'O'
         | 'P' | 'Q' | 'R' | 'S' | 'T'
         | 'U' | 'V' | 'W' | 'Z' | 'Y'
         | 'a' | 'b' | 'c' | 'd' | 'e'
         | 'f' | 'g' | 'h' | 'i' | 'j'
         | 'k' | 'l' | 'm' | 'n' | 'o'
         | 'p' | 'q' | 'r' | 's' | 't'
         | 'u' | 'v' | 'w' | 'x' | 'y'
         | 'Z' | 'z' | '_' =>
        putChar(ch)
        nextChar()
        getIdentRest()
        if (ch == '"' && token == IDENTIFIER) token = INTERPOLATION
      case '!' | '@' | '#' | '$' | '%'
         | '^' | '&' | '*' | '+' | '-'
         | '?' | ':' | '=' | '|' | '\\'
        =>
        putChar(ch)
        nextChar()
        getOperatorRest()
      case '/' =>
        if skipComment() then
          fetchToken()
        else
          putChar(ch)
          getOperatorRest()
      case '0' =>
        nextChar()
        ch match {
          case 'x' | 'X' => base = 16; nextChar()
          case 'b' | 'B' => base = 2;  nextChar()
          case _         => base = 10; putChar('0')
        }
        if (base != 10 && digit2int(ch, base) < 0)
          error(er"Invalid number literal", offset)
        getNumber()
      case '1' | '2' | '3' | '4' | '5'
         | '6' | '7' | '8' | '9' =>
        base = 10
        getNumber()
      case '\"' =>
        def _set() = currentRegion = InString(currentRegion)
        if (token == INTERPOLATION) {
          nextRawChar()
          if (ch == '\"') {
            nextChar()
            token = STRINGLIT
            strval = ""
          } else {
            _set()
          }
        } else {
          nextChar()
          if (ch == '\"') {
            nextChar()
            if (ch == '\"') {
              nextRawChar()
              getRawStringLit()
            } else {
              token = STRINGLIT
              strval = ""
            }
          } else getStringLit()
        }
      //case '\'' =>
      case '.' =>
        nextChar()
        if ('0' <= ch && ch <= '9') {
          putChar('.'); getFraction(); setStrVal()
        } else token = DOT
      case ',' => nextChar(); token = COMMA
      case ';' => nextChar(); token = SEMI
      case '~' => nextChar(); token = TILDE
      case '(' => nextChar(); token = LPAREN
      case ')' => nextChar(); token = RPAREN
      case '[' => nextChar(); token = LBRACKET
      case ']' => nextChar(); token = RBRACKET
      case '<' => nextChar(); token = LSHARP
      case '>' => nextChar(); token = RSHARP
      case '{' => nextChar(); token = LBRACE
      case '}' => nextChar(); token = RBRACE
      case _ =>
    }
  }

  def skipComment(): Boolean =
    def nextChar(): Unit  = { Scanner.this.nextChar() }
    def skipLine(): Unit = { nextChar(); if ((ch != CR) && (ch != LF)) {skipLine()} }
    @tailrec def skipComment(): Unit =
      if (ch == '/') {
        nextChar()
        if (ch == '*') nestedComment()
        skipComment()
      } else if (ch == '*') {
        while ({ nextChar(); ch == '*' }) ()
        if (ch == '/') nextChar()
        else skipComment()
      } else {
        nextChar()
        skipComment()
      }
    def nestedComment() = { nextChar(); skipComment() }
    val start = lastCharOffset
    nextChar()
    if (ch == '/') { skipLine(); true }
    else if (ch == '*') { nextChar(); skipComment(); true}
    else false


  import Character.{isHighSurrogate, isLowSurrogate, isUnicodeIdentifierPart}
  import Character.{isUnicodeIdentifierStart, isValidCodePoint, toCodePoint}

  private def toUnicode(x: Char): String = {val s=x.toInt.toHexString; "\\u" + "0" * (4 - s.length) + s}

  private def isSupplementary(hi: Char, test: Int => Boolean, strict: Boolean = true): Boolean =
    isHighSurrogate(hi) && {
      var res = false
      val low = lookaheadChar()
      if isLowSurrogate(low) then
        val codept = toCodePoint(hi, low)
        if isValidCodePoint(codept) then
          if test(codept) then
            putChar(hi)
            putChar(low)
            nextChar()
            nextChar()
            res = true
        else
          error(er"Illegal character '${toUnicode(hi)}${toUnicode(low)}'", offset)
      else if !strict then
        putChar(hi)
        nextChar()
        res = true
      else
        error(er"Illegal character '${toUnicode(hi)}' missing low surrogate", offset)
      res
    }
  private def atSupplementary(ch: Char, test: Int => Boolean): Boolean =
    isHighSurrogate(ch) && {
      val hi = ch
      val lo = lookaheadChar()
      isLowSurrogate(lo) && {
        val cp = toCodePoint(hi, lo)
        isValidCodePoint(cp) && test(cp)
      }
    }

  def skipParens(multiple: Boolean = true): Unit =
    val opening = token
    nextToken()
    while token != EOF && token != opening + 1 do
      if token == opening && multiple then skipParens() else nextToken()
    nextToken()

  def getIdentRest(): Unit = ch match {
    case 'A' | 'B' | 'C' | 'D' | 'E'
       | 'F' | 'G' | 'H' | 'I' | 'J'
       | 'K' | 'L' | 'M' | 'N' | 'O'
       | 'P' | 'Q' | 'R' | 'S' | 'T'
       | 'U' | 'V' | 'W' | 'Z' | 'Y'
       | 'a' | 'b' | 'c' | 'd' | 'e'
       | 'f' | 'g' | 'h' | 'i' | 'j'
       | 'k' | 'l' | 'm' | 'n' | 'o'
       | 'p' | 'q' | 'r' | 's' | 't'
       | 'u' | 'v' | 'w' | 'x' | 'y'
       | 'Z' | 'z'
       | '0' | '1' | '2' | '3' | '4'
       | '5' | '6' | '7' | '8' | '9'
      =>
      putChar(ch)
      nextChar()
      getIdentRest()
    case '_' =>
      putChar(ch)
      nextChar()
      getIdentOrOperatorRest()
    case _ =>
      if isUnicodeIdentifierPart(ch) then
        putChar(ch)
        nextChar()
        getIdentRest()
      else if isSupplementary(ch, isUnicodeIdentifierPart) then
        getIdentRest()
      else
        finishNamed()
  }

  def getOperatorRest(): Unit = (ch: @switch) match {
    case '!' | '@' | '#' | '$' | '%'
       | '^' | '&' | '*' | '+' | '-'
       | '?' | ':' | '=' | '|' | '\\'
      =>
      putChar(ch)
      nextChar()
      getOperatorRest()
    case '/' =>
      val nx = lookaheadChar()
      if nx == '/' || nx == '*' then finishNamed()
      else
        putChar(ch)
        nextChar()
        getOperatorRest()
    case _ =>
      if isSpecial(ch) then
        putChar(ch)
        nextChar()
        getOperatorRest()
      else if isSupplementary(ch, isSpecial) then getOperatorRest()
      else finishNamed()
  }

  def getIdentOrOperatorRest(): Unit =
    if (isIdentifierPart(ch) || isSupplementary(ch, isIdentifierPart)) getIdentRest() else getOperatorRest()



  def lookahead: TokenData =
    if next.token == EMPTY then
      assert(token != INTERPOLATION)
      peekAhead()
      reset()
    next

  //def isSoftModifier: Boolean =
  //  token == IDENTIFIER
  //  && softModifierNames.contains(name)
  //    || name == nme.erased && erase
      

  // Literals /////////////////////////////////////////////////////////////////
  private def getStringLit() =
    getLitChars('"')
    if ch == '"' then
      setStrVal()
      nextChar()
      token = STRINGLIT
    else error(er"unclosed string literal", offset)
  
  @tailrec
  private def getRawStringLit(): Unit =
    if ch == '\"' then
      nextRawChar()
      setStrVal()
      token = STRINGLIT
    else if ch == SU then
      error(er"unclosed string literal", offset)
    else
      putChar(ch)
      nextRawChar()
      getRawStringLit()

  @tailrec
  private def getStringPart(): Unit =
    if ch == '"' then
      nextChar()
      setStrVal()
      token = STRINGLIT
    else if ch == '\\' then
      putChar(ch)
      nextRawChar()
      if ch == '"' || ch == '\\' then
        putChar(ch)
        nextRawChar()
      getStringPart()
    else if ch == '$' then // Im about 40% sure that you can change the interpolator by changing these
      def getInterpolatedIdentRest(hasSupplement: Boolean): Unit = {
        @tailrec def loopRest(): Unit =
          if ch != SU && isUnicodeIdentifierPart(ch) then
            putChar(ch)
            nextRawChar()
            loopRest()
          else if atSupplementary(ch, isUnicodeIdentifierPart) then
            putChar(ch); nextRawChar()
            putChar(ch); nextRawChar()
            loopRest()
          else finishNamedTk(IDENTIFIER, target = next)
        end loopRest
        setStrVal()
        token = INTERPOLATION
        next.lastOffset = charOffset - 1
        next.offset = charOffset - 1
        putChar(ch)
        nextRawChar()
        if hasSupplement then
          putChar(ch)
          nextRawChar()
        loopRest()
      }
      nextRawChar()
      if ch == '$' || ch == '"' then
        putChar(ch)
        nextRawChar()
        getStringPart()
      else if ch == '{' then
        setStrVal()
        token = INTERPOLATION
      else if isUnicodeIdentifierStart(ch) || ch == '_' then
        getInterpolatedIdentRest(false)
      else if atSupplementary(ch, isUnicodeIdentifierStart) then
        getInterpolatedIdentRest(true)
      else
        error(er"Invalid string interpolation", offset - 2)
        putChar('$')
        getStringPart()
    else
      val isUnclosed = !isUnicodeEscape && ch == SU 
      if isUnclosed then
        error(er"unclosed string literal", offset)
      else
        putChar(ch)
        nextRawChar()
        getStringPart()
  end getStringPart

  private def fetchStringPart() =
    offset = charOffset - 1
    getStringPart()

  protected def getLitChar(): Unit =
    if ch == '\\' then
      nextChar()
      charEscape()
    else if !isSupplementary(ch, _ => true, strict = false) then
      putChar(ch)
      nextChar()

  protected def getLitChars(delimiter: Char) =
    while ch != delimiter && !isAtEnd && (ch != SU && ch != CR && ch != LF || isUnicodeEscape) do
      getLitChar()

  private def charEscape(): Unit =
    var bump = true
    ch match
      case 'b'  => putChar('\b')
      case 't'  => putChar('\t')
      case 'n'  => putChar('\n')
      case 'f'  => putChar('\f')
      case 'r'  => putChar('\r')
      case '\"' => putChar('\"')
      case '\'' => putChar('\'')
      case '\\' => putChar('\\')
      case 'u' | 'U' => uEscape(); bump = false
      case _ => invalidEscape()
    if bump then nextChar()

  private def uEscape(): Unit =
    while ch == 'u' || ch == 'U' do nextChar()
    var i, cp = 0
    while i < 4 do
      val digit = digit2int(ch, 16)
      if digit < 0 then
        error(er"invalid character in unicode escape sequence\naccepted characters are 0 to 9, a to f (uppercase or lowercase)", charOffset - 1)
        putChar(ch)
        return
      val shift = (3 - i) * 4
      cp += digit << shift
      nextChar()
      i += 1

    putChar(cp.asInstanceOf[Char])

  private def invalidEscape(): Unit =
    error(er"Invalid escape character", charOffset - 1)
    putChar(ch)

  protected def getFraction(): Unit =
    token = DOUBLELIT
    while '0' <= ch && ch <= '9' do
      putChar(ch)
      nextChar()
    checkNoTrailingSeparator()

    if ch == 'd' || ch == 'D' then
      putChar(ch)
      nextChar()
      token = DOUBLELIT
    else if ch == 'f' || ch == 'F' then
      putChar(ch)
      nextChar()
      token = FLOATLIT
    checkNoLetter()


  def checkNoLetter(): Unit =
    if (isIdentifierPart(ch) && ch >= ' ') error(er"Invalid number literal", offset)

  protected def getNumber(): Unit =
    while digit2int(ch, base) >= 0 do
      putChar(ch)
      nextChar()
    checkNoTrailingSeparator()
    token = INTLIT
    if base == 10 && ch == '.' then
      val lch = lookaheadChar()
      if '0' <= lch && lch <= '9' then
        putChar('.')
        nextChar()
        getFraction()
    else (ch: @switch) match
      case 'f' | 'F' | 'd' | 'D' =>
        if base eq 10 then getFraction()
      case 'l' | 'L' =>
        nextChar()
        token = LONGLIT
      case _ =>
    checkNoTrailingSeparator()
    setStrVal()

  def finishCharLit(): Unit =
    nextChar()
    token = CHARLIT
    setStrVal()

  def charLitOr(fn: => Token): Unit =
    putChar(ch)
    nextChar()
    if ch == '\'' then finishCharLit()
    else
      token = fn
      strval = Objects.toString(name)
      clearLit()

  //override def toString: String =
  //  showTokenDetailed(token) + {
  //    if identifierTokens.contains(token) then s" $name"
  //    else if literalTokens.contains(token) then s" $strval"
  //    else ""
  // }



  // Initialization
  nextChar()
  nextToken()
}
