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

          if (tokenStr indexOf stack.map(_.toChar).mkString("") ne -1)
            token = tokenStr indexOf stack.map(_.toChar).mkString("")

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
          // FIXME: I dont think this actually handles the arrows :(
          if      ((token eq '<') && (content(pos + 1) eq '-')) { token = LARRS; incChar(); incChar() }
          else if ((token eq '<') && (content(pos + 1) eq '=')) { token = LARRB; incChar(); incChar() }
          else if ((token eq '=') && (content(pos + 1) eq '>')) { token = RARRB; incChar(); incChar() }
          else if ((token eq '-') && (content(pos + 1) eq '>')) { token = RARRS; incChar(); incChar() }
          else {
            token = IDENTIFIER
            push(char)
            if (stack.exists(_.toChar.isLetter)) { popAll(); push(char) }

            if (tokenStr indexOf stack.map(_.toChar).mkString("") ne -1)
              token = tokenStr indexOf stack.map(_.toChar).mkString("")

            incChar()
          }
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
        if ((base ne 10) && (char ne '_') && (digit_int() < 0))
          Error(er"invalid number literal", path)
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
      case '.' => // FIXME: decimals dont work. no idea why. dont care much rn
        incChar()
        if ('0' <= char && char <= '9') {// decimal handler
          push('.') // TODO: I think more needs to be done here
        }
        else token = DOT
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
          println(s"ahh ${char}")
    }
  
  def skipComment(): Boolean = {
    def _skipLine(): Unit =
    { incChar()
      if (char ne CR) && (char ne LF) then _skipLine() }

    def _nest(): Unit =
    { incChar(); _skipComment() }

    @tailrec def _skipComment(): Unit =
      if (char eq '/') {
        incChar()
        if (char eq '*') _nest()
        _skipComment()
      } else if (char eq '*') {
        while ({ incChar(); char eq '*' }) (/*yuh*/)
        if (char eq '/') incChar()
        else _skipComment()
      } else {
        incChar()
        _skipComment()
      }

    incChar()
    if char eq '/' then
      _skipLine()
      return true
    else if char eq '*' then
      incChar()
      _skipComment()
      return true
    else
      return false
  }

  def getNumber(): Unit = {
    if ((token ne STRINGLIT) && (token ne CHARLIT)) {
      def _get(x: Byte): Byte = x match {
        case '0' => 0
        case '1' => 1
        case '2' if (base > 2) => 2
        case '3' if (base > 2) => 3
        case '4' if (base > 2) => 4
        case '5' if (base > 2) => 5
        case '6' if (base > 2) => 6
        case '7' if (base > 2) => 7
        case '8' if (base > 2) => 8
        case '9' if (base > 2) => 9
        case '2' | '3' | '4' | '5' | '6'
           | '7' | '8' | '9' => -2
        case 'a' | 'A' if (base eq 16) => 10
        case 'b' | 'B' if (base eq 16) => 11
        case 'c' | 'C' if (base eq 16) => 12
        case 'd' | 'D' if (base eq 16) => 13
        case 'e' | 'E' if (base eq 16) => 14
        case 'f' | 'F' if (base eq 16) => 15
        case 'a' | 'A' | 'b' | 'B' | 'c'
           | 'C' | 'd' | 'D' | 'e' | 'E'
           | 'f' | 'F' => -2
        case '.' => -3
        case _ => -1
      }

      token = INTLIT
      val x = _get(char)
      if x eq -2 then Error(er"digit $char does not map to base $base", path)
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
  def incChar(): Unit = pos += 1; char = content(pos)
  def setToken(t: Int): Unit = 
    if ((token ne CHARLIT) && (token ne STRINGLIT)) { token = t; popAll() }
    else if (token eq STRINGLIT) push(char)
    else if (stack.isEmpty) push(char)
    else Error(er"char literals cannot have more than 1 character", path)
    incChar()
  inline def push(c: Byte): Unit = stack = stack :+ c
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
