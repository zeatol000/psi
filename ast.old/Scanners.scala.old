/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import psi.cc.ast.*
import psi.cc.ast.Tokens.*
import psi.cc.utils.*
import scala.collection.*
import scala.annotation.tailrec

case class TD (
  stack: mutable.Seq[Byte],
  token: Int,
  start: Int
  )

private[ast]
class Scanner
(
  val content: Seq[Byte],
  val path: String
)(using Context)
{
  inline val CR = 0x000D // Carriage return -- \r
  inline val LF = 0x000A // Line Feed -- \n

  var token = EMPTY // change to newest token
  var char: Byte = 0
  var pos: Int = 0
  var stack: mutable.Seq[Byte] = mutable.Seq()
  var start: Int = 0
  var base: Byte = 10 

  def fetchToken(): Unit =
    if pos >= content.length then 
      token = EOF
      return
    char = content(pos)
    char match {
      case ' ' | '\t'| CR | LF =>
        if ((token ne CHARLIT) && (token ne STRINGLIT)) {
          incChar()
          popAll()
          fetchToken()
        } else if (token eq CHARLIT) {
          if (stack.isEmpty) push(char)
          else Error(er"char literals cannot have more than 1 character", path)
          incChar()
        } else {
          push(char)
          incChar()
        }
      case 'A' | 'B' | 'C' | 'D' | 'E' |
           'F' | 'G' | 'H' | 'I' | 'J' |
           'K' | 'L' | 'M' | 'N' | 'O' |
           'P' | 'Q' | 'R' | 'S' | 'T' |
           'U' | 'V' | 'W' | 'X' | 'Y' |
           'Z' | '$' | '_' |
           'a' | 'b' | 'c' | 'd' | 'e' |
           'f' | 'g' | 'h' | 'i' | 'j' |
           'k' | 'l' | 'm' | 'n' | 'o' |
           'p' | 'q' | 'r' | 's' | 't' |
           'u' | 'v' | 'w' | 'x' | 'y' |
           'z' =>
        if (token ne STRINGLIT) {
          token = IDENTIFIER
          push(char)

          if (stack.exists(_ match { case
            '!' | '@' | '#' | '%' | '~' |
            '^' | '*' | '+' | '-' | '?' |
            ':' | '=' | '&' | '|' | '\\'|
            '/' => true; case _ => false}))
          { popAll(); push(char) }

          val index = tokenStr indexOf stack.map(_.toChar).mkString("")
          if (index ne -1)
            token = index

          incChar()
        } else if (token eq STRINGLIT) {
          push(char)
          incChar()
        } else if (token eq CHARLIT) {
          if (stack.isEmpty) push(char)
          else Error(er"char literals cannot have more than 1 character", path)
          incChar()
        }

      case '!' | '@' | '#' | '%' | '~' |
           '^' | '*' | '+' | '-' | '?' |
           ':' | '=' | '&' | '|' | '\\'|
           '>' | '<' =>
        if ((token ne STRINGLIT) && (token ne CHARLIT)) {
          token = IDENTIFIER
          val current = char
          push(char)
          if (stack.exists(_.toChar.isLetter)) { popAll(); push(char) }

          if ({ incChar(); char match {
            case '>' if current == '-' || current == '=' => true
            case '-' if current == '<' => true
            case '=' if current == '=' => true
            case  _  => false
          }}) { push(char) } else { decChar() }

          val index = tokenStr indexOf stack.map(_.toChar).mkString("")
          if (index ne -1)
            token = index

          incChar()

        } else {
          push(char)
          incChar()
        }
      case '/' =>
        if skipComment() then fetchToken()
        else push(char)
      case '0' =>
        incChar()
        char match {
          case 'x' | 'X' => base = 16; incChar()
          case 'b' | 'B' => base = 2;  incChar()
          case 'd' | 'D' => base = 10; incChar()
          case _         => base = 10
        }
        if ((base != 10) /*&& (char ne '_')*/ && (digit_int() < 0)) {
          Error(er"invalid number literal", path)
        }
        decChar()
        getNumber()
      case '1' | '2' | '3' | '4' | '5' |
           '6' | '7' | '8' | '9' =>
        base = 10
        getNumber()
      case '\"' => // String lit
        if (token eq STRINGLIT) {
          if (content(pos - 1) eq '\\')   push(char)
          else                            token = EMPTY
          incChar()
        } else if (token eq CHARLIT) {
          push(char)
          incChar()
        } else {
          token = STRINGLIT
          incChar()
        }
      case '\'' => // Char lit
        if (token eq CHARLIT) {
          if (stack.isEmpty) {
            if (content(pos - 1) eq '\\')
              push(char)
            else Error(er"char literals cannot have 0 characters", path)
          } else if (content(pos - 1) eq '\\')
            Error(er"char literals cannot have more than 1 character", path)
          else
            token = EMPTY
          incChar()
        } else if (token eq STRINGLIT) {
          push(char)
          incChar()
        } else {
          token = CHARLIT
          incChar()
        }
      case '.' =>
        val next = if (content.length > pos + 1) content(pos + 1) else EOF
        if ('0' <= next && next <= '9') {// decimal handler
          push('.') // TODO: I think more needs to be done here
          token = DOUBLELIT
        }
        else {token = DOT; popAll()}
        incChar()
      case ';' => setToken(SEMI     )
      case ',' => setToken(COMMA    )
      case '(' => setToken(LPAREN   )
      case ')' => setToken(RPAREN   )
      case '{' => setToken(LBRACE   )
      case '}' => setToken(RBRACE   )
      case '[' => setToken(LBRACKET )
      case ']' => setToken(RBRACKET )
      //case '<' => setToken(LSHARP   ) probably need special handling
      //case '>' => setToken(RSHARP   )
      case _ => // TODO: this
          Error(er"Unsupported character $char\ncurrently only UTF-8 is supported", path)
          incChar()
    }
  
  def skipComment(): Boolean = {
    def _skipLine(): Unit =
    { incChar()
      if (char != CR) && (char != LF) then _skipLine() }

    def _nest(): Unit =
    { incChar(); _skipComment() }

    @tailrec def _skipComment(): Unit =
      if (char == '/') {
        incChar()
        if (char == '*') _nest()
        _skipComment()
      } else if (char == '*') {
        while ({ incChar(); char == '*' }) (/*yuh*/)
        if (char == '/') incChar()
        else _skipComment()
      } else if (char == EOF) {
        return
      } else {
        incChar()
        _skipComment()
      }

    incChar()
    if (char == '/') {
      _skipLine()
      return true
    }
    else if (char == '*') {
      incChar()
      _skipComment()
      return true
    }
    else {
      println("no line skip")
      return false
    }
  }

  def getNumber(): Unit = {
    if ((token ne STRINGLIT) && (token ne CHARLIT)) {

      if (stack.isEmpty)
        token = INTLIT
      val x = digit_int()
      if x eq -1 then Error(er"digit $char does not map to base $base", path)
      else push(char)
      incChar()

      /*_type match {
        case INTLIT | LONGLIT | -1 =>
          val abs = temp.foldLeft(0L)((acc, dig) => acc * base + dig)
          val fin = if (stack eq mutable.Seq[Byte]('-')) then -abs else abs
          if _type ne LONGLIT then
            intBuf = fin.toInt
            token = INTLIT
          else 
            longBuf = fin
            token = LONGLIT
        case DOUBLELIT | FLOATLIT =>
          val int = temp.foldLeft(0.0)((acc, dig) => acc * base + dig)
          val dec = dtemp.foldRight(0.0)((dig, acc) => (dig + acc) / base)
          val abs = int + dec
          val fin = if (stack eq mutable.Seq[Byte]('-')) then -abs else abs
          if _type eq FLOATLIT then
            floatBuf = fin.toFloat
            token = FLOATLIT
          else
            doubleBuf = fin
            token = DOUBLELIT
      }*/
    } else if (token eq STRINGLIT) {
      push(char)
    } else {
      if (stack.length eq 1) Error(er"char literal cannot have a more than 1 character", path)
      else push(char)
    }
  }

  // Util /////////////////////////////////////////////////////////////////////
  def incChar(): Unit = {
    pos += 1
    try char = content(pos)
    catch {
      case e: IndexOutOfBoundsException => char = EOF
    }
  }
  def decChar(): Unit = {
    pos -= 1
    try char = content(pos)
    catch {
      case e: IndexOutOfBoundsException => char = EMPTY
    }
  }

  def setToken(t: Int): Unit = 
    if ((token ne CHARLIT) && (token ne STRINGLIT)) { token = t; popAll(); start = pos }
    else if (token eq STRINGLIT) push(char)
    else if (stack.isEmpty) push(char)
    else Error(er"char literals cannot have more than 1 character", path)
    incChar()

  def push(c: Byte): Unit =
    if (stack.isEmpty) start = pos
    stack = stack :+ c
  
  inline def popAll(): Unit = stack = mutable.Seq()

  def digit_int(): Byte = base match {
    case 10 => char match {
      case '0' => 0; case '1' => 1; case '2' => 2
      case '3' => 3; case '4' => 4; case '5' => 5
      case '6' => 6; case '7' => 7; case '8' => 8
      case '9' => 9; case _ => -1
    }
    case 2 => char match {
      case '0' => 0; case '1' => 1; case _ => -1
    }
    case 16 => char match {
      case '0' => 0; case '1' => 1; case '2' => 2
      case '3' => 3; case '4' => 4; case '5' => 5
      case '6' => 6; case '7' => 7; case '8' => 8
      case '9' => 9; case 'a' | 'A' => 10;
      case 'b' | 'B' => 11; case 'c' | 'C' => 12
      case 'd' | 'D' => 13; case 'e' | 'E' => 14
      case 'f' | 'F' => 15; case _ => -1
    }
  }
}
