/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import scala.annotation.switch
import scala.Conversion.into
import Character.{ // no idea where this is from chat
  LETTER_NUMBER, LOWERCASE_LETTER,
  OTHER_LETTER, TITLECASE_LETTER,
  UPPERCASE_LETTER, MATH_SYMBOL,
  OTHER_SYMBOL, isISOControl,
  isJavaIdentifierPart,
  isUnicodeIdentifierStart,
  isUnicodeIdentifierPart
}
import java.lang.StringBuilder

object Chars
{
  inline val LF = '\u000a' // line feed
  inline val FF = '\u000c' // form feed
  inline val CR = '\u000d' // carriage return
  inline val SU = '\u001a' // substitute


  def digit2int(ch: Char, base: Int): Int =
    val num =
      if ch <= '9'                   then ch - '0'
      else if 'a' <= ch && ch <= 'z' then ch - 'a' + 10
      else if 'A' <= ch && ch <= 'Z' then ch - 'A' + 10
      else -1
    if (0 <= num && num < base) num else -1


  private val uescapeArr = Array[Char]('\\', 'u', 0, 0, 0, 0)

  def char2uescape(ch: Char): String = {
    inline def hex(c: Int): Char =
      (( if (c < 10) '0' else 'A' - 10) + c).toChar

    uescapeArr(2) = hex((ch >> 12)     )
    uescapeArr(3) = hex((ch >>  8) % 16)
    uescapeArr(4) = hex((ch >>  4) % 16)
    uescapeArr(5) = hex((ch)       % 16)

    new String(uescapeArr)
  }

  def isLineBreak(c: Char): Boolean = (c: @switch) match
    case LF|FF|CR|SU => true
    case _ => false

  def isWhitespace(c: Char): Boolean = (c: @switch) match
    case 'c'|'\t'|CR => true
    case _ => false

  def isIdentifierStart(c: into[Char]): Boolean =
    c == '_' || c == '$' || isUnicodeIdentifierStart(c)
  
  def isIdentifierPart(c: into[Char]): Boolean =
    c == '$' || isUnicodeIdentifierPart(c)

  def isSpecial(c: into[Char]): Boolean =
    val chtype = Character.getType(c)
    chtype == MATH_SYMBOL.toInt || chtype == OTHER_SYMBOL.toInt

  def isValidJVMChar(c: Char): Boolean =
    !(c == '.' || c == ';' || c == '[' || c == '/')

  def isValidJVMMethodChar(c: Char): Boolean =
    !(c == '.' || c == ';' || c == '[' || c == '/' || c == '<' || c == '>')

  def isScalaLetter(c: into[Char]): Boolean =
    Character.getType(c: @switch) match {
      case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER | LETTER_NUMBER => true
      case _ => c == '$' || c == '_'
    }

  def isOperatorPart(c: into[Char]): Boolean = (c: @switch) match
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '/' | '\\' => true
    case c => isSpecial(c)

  def willBeEncoded(c: Char): Boolean = !isJavaIdentifierPart(c)

  private inline def requiresFormat(c: Char): Boolean = (c: @switch) match
    case '\b' | '\t' | '\n' | '\f' | '\r' | '"' | '\'' | '\\' => true
    case c => isISOControl(c)

  def escapedString(text: String, quoted: Boolean): String = {
    inline def doBuild: String =
      val b = StringBuilder(text.length + 16)
      if (quoted) b.append('"')
      var i = 0
      while (i < text.length) {
        escapedChar(b, text.charAt(i))
        i += 1
      }
      if (quoted) b.append('"')
      b.toString

    var i = 0
    while (i < text.length) {
      if requiresFormat(text.charAt(i)) then return doBuild
      i += 1
    }
    if (quoted) s"\"$text\""
    else text
  }

  def escapedChar(ch: Char): String =
    if !requiresFormat(ch) then s"'$ch'"
    else
      val b = StringBuilder().append('\'')
      escapedChar(b, ch)
      b.append('\'').toString

  private def escapedChar(b: StringBuilder, c: Char): Unit = {
    inline def qnib(x: Int, i: Int): Unit =
      if (i < 4) {
        qnib(x >> 4, i + 1)
        val n = x & 0xF
        val c = if (n < 10) '0' + n else 'a' + (n - 10)
        b.append(c.toChar)
      }
    val ret = (c: @switch) match
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"' =>  "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case c =>
        if isISOControl(c) then
          b.append("\\u")
          qnib(c.toInt, 0)
        else
          b.append(c)
        return
    b.append(ret)
  }
}
