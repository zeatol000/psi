/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package parser

import psi.cc.*
import utils.*
import ast.*
import ast.Trees.*
import Tokens.*

private[parser]
class Parser
(Content: Seq[Char], Path: String)(using Context)
extends ParserCommon(Content, Path)
{
  val ast: Tree = null
  val sc: Scanner = new Scanner( Content, Path )

  def parse: Unit = {

    if Content.isEmpty then
      Error(er"File content returned null\nCannot continue", Path)
      return

    Content.foreach(print)
  }

  def IsIdent           = sc.isIdent
  def IsIdent(n: Name)  = sc.isIdent(n)
  def IsArrow           = sc.isArrow
  def IsLiteral         = LiteralTokens.contains(sc.token)
  def IsNumericLit      = NumericLitTokens.contains(sc.token)
  def IsTemplateIntro   = TemplateIntroTokens.contains(sc.token)
  def IsDeclareIntro    = DeclareIntroTokens.contains(sc.token)
  def IsDeclareNext     = DeclareIntroTokens.contains(sc.lookahead.token)
  def IsStatSeqEnd      = sc.isNestEnd || sc.token == EOF || sc.token == RPAREN
  def MustStartStat     = MustStartStatTokens.contains(sc.token)
  def IsModifier        = ModifierTokens.contains(sc.token)
  def IsBindingIntro    = sc.token match {
    case USCORE     => true
    case IDENTIFIER => sc.lookahead.isArrow
    case LPAREN     => 
      val la = sc.LookaheadScanner()
      la.skipParens()
      la.isArrow
    case _          => false}
  def IsExprIntro       = CanStartExprTokens.contains(sc.token)
  def IsDefineIntro     = DefineIntroTokens.contains(sc.token)
  def IsStatSep         = sc.isStatSep
  def IsSplice          =
    sc.token == IDENTIFIER && sc.name(0) == '$' && {
      if sc.name.length == 1 then sc.lookahead.token == LBRACE
      else (staged & StageKind.Quoted) != 0
    }
  
  def skip() =
    sc.skip()
    lastErrorOffset = sc.offset

  def accept(token: Int): Int =
    val offset = sc.offset
    if sc.token != token then sc.error(er"Expected $token but found ${sc.token}", offset)
    if sc.token == token then sc.nextToken()
    offset

  def accept(name: Name): Int =
    val offset = sc.offset
    if !IsIdent(name) then sc.error(er"'$name' expected", offset)
    if IsIdent(name) then sc.nextToken()
    offset

  def acceptColon(): Int =
    val offset = sc.offset
    val current = sc.token
    sc.nextToken()
    if sc.token == COLON then // ::
      sc.nextToken()
      offset
    else // :
      offset

  def acceptStatSep(): Unit =
    if sc.isNewLine then sc.nextToken() else accept(SEMI)

  private var staged = StageKind.None
}
