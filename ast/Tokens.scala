/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

/**
 * This file represents the all of the keywords of Psi and pushes them into a buffer
 *
 */

import scala.collection.immutable.BitSet

abstract class Tokens
{ 
  def maxToken: Int

  def tokenRange(l: Int, h: Int): BitSet = BitSet(l to h *)

  val tokenStr = new Array[String](maxToken + 1)
  def push(token: Byte, str: String): Unit = tokenStr(token) = str

  val keywords: BitSet
  def isKeyword(x: Int): Boolean = keywords.contains(x)

  /** strings that begin with ! mean that isn't the literal keyword
   *! DO NOT REPLACE TOKENS. All values must be positive          */

  /* AST-specific keywords -- you dont have these in normal files except eof ig */
  inline val IDENTIFIER  = 0;   push( IDENTIFIER,  "!identifier "  ) // Points to the name section of the AST
  inline val EMPTY       = 1;   push( EMPTY,       "!empty      "  ) // Missing token / utility
  inline val ERROR       = 2;   push( ERROR,       "!token error"  ) // Error identifier
  inline val EOF         = 3;   push( EOF,         "!eof        "  ) // End of file
  inline val APPLY       = 4;   push( APPLY,       "!apply      "  ) // run a function
  inline val SELECT      = 5;   push( SELECT,      "!select     "  ) // select member of value
  
  /* literals */
  inline val CHARLIT     = 10;  push( CHARLIT,     "!char lit  "   ) // ' '
  inline val INTLIT      = 11;  push( INTLIT,      "!int lit   "   ) // Int, Short, and Byte
  inline val LONGLIT     = 12;  push( LONGLIT,     "!long lit  "   ) // Long
  inline val FLOATLIT    = 13;  push( FLOATLIT,    "!float lit "   ) // Float
  inline val DOUBLELIT   = 14;  push( DOUBLELIT,   "!double lit"   ) // Double
  inline val STRINGLIT   = 15;  push( STRINGLIT,   "!string lit"   ) // " "
  inline val NULL        = 16;  push( NULL,        "null"          )
  inline val TRUE        = 17;  push( TRUE,        "true"          )
  inline val FALSE       = 18;  push( FALSE,       "false"         )
  
  /* main keywords */
  inline val IF          = 20;  push( IF,          "if"            )
  inline val ELSE        = 21;  push( ELSE,        "else"          )
  inline val FOR         = 22;  push( FOR,         "for"           )
  inline val WHILE       = 23;  push( WHILE,       "while"         )
  inline val NEW         = 24;  push( NEW,         "new"           )
  inline val THIS        = 25;  push( THIS,        "this"          )
  inline val SUPER       = 26;  push( SUPER,       "super"         )
  inline val CASE        = 27;  push( CASE,        "case"          )
  inline val ABSTRACT    = 28;  push( ABSTRACT,    "abstract"      )
  inline val FINAL       = 29;  push( FINAL,       "final"         )
  inline val PRIVATE     = 30;  push( PRIVATE,     "private"       )
  inline val INHERITED   = 31;  push( INHERITED,   "inherited"     ) // supposed to be "protected" but i cant get it to stop thinking its trying to say pro
  inline val OVERRIDE    = 32;  push( OVERRIDE,    "override"      )
  inline val EXTENDS     = 33;  push( EXTENDS,     "extends"       )
  inline val AS          = 34;  push( AS,          "as"            )
  inline val SEALED      = 35;  push( SEALED,      "sealed"        )
  inline val TRY         = 36;  push( TRY,         "try"           )
  inline val CATCH       = 37;  push( CATCH,       "catch"         )
  inline val FINALLY     = 38;  push( FINALLY,     "finally"       )
  inline val RETURN      = 39;  push( RETURN,      "ret"           )
  inline val THROW       = 40;  push( THROW,       "throw"         )

  /* data types */
  inline val VAL         = 50;  push( VAL,         "val"           )
  inline val VAR         = 51;  push( VAR,         "var"           )
  inline val PRO         = 52;  push( PRO,         "pro"           )
  inline val SUB         = 53;  push( SUB,         "sub"           )
  inline val CO          = 54;  push( CO,          "co"            )
  inline val FN          = 55;  push( FN,          "fn"            )
  inline val OP          = 56;  push( OP,          "op"            )
  inline val PACKAGE     = 57;  push( PACKAGE,     "package"       )
  inline val OBJ         = 58;  push( OBJ,         "obj"           )
  inline val CLASS       = 59;  push( CLASS,       "class"         )
  inline val TRAIT       = 60;  push( TRAIT,       "trait"         )
  inline val TYPE        = 61;  push( TYPE,        "type"          )
  inline val MOD         = 62;  push( MOD,         "mod"           )

  /* punctuation */
  inline val DOT         = 70;  push( DOT,         "."             )
  inline val COMMA       = 71;  push( COMMA,       ","             )
  inline val SEMI        = 72;  push( SEMI,        ";"             )
  inline val USCORE      = 73;  push( USCORE,      "_"             )
  inline val ASTERISK    = 74;  push( ASTERISK,    "*"             )
  inline val TILDE       = 75;  push( TILDE,       "~"             )
  inline val NEWLINE     = 76;  push( NEWLINE,     "!newline"      ) // Going to be really useful in figuring out whether something is the end of a statement
  inline val EQUALS      = 77;  push( EQUALS,      "="             )
  inline val COLON       = 78;  push( COLON,       ":"             )
//inline val DOUBLECOLON = 79;  push( DOUBLECOLON, "::"            )
  inline val LARRS       = 80;  push( LARRS,       "<-"            )
//inline val LARRB       = 81;  push( LARRB,       "<="            )
  inline val RARRS       = 82;  push( RARRS,       "->"            )
  inline val RARRB       = 83;  push( RARRB,       "=>"            )
  
  /* bracing */
  inline val LPAREN      = 90;  push( LPAREN,      "("             )
  inline val RPAREN      = 91;  push( RPAREN,      ")"             )
  inline val LBRACKET    = 92;  push( LBRACKET,    "["             )
  inline val RBRACKET    = 93;  push( RBRACKET,    "]"             )
  inline val LBRACE      = 94;  push( LBRACE,      "{"             )
  inline val RBRACE      = 95;  push( RBRACE,      "}"             )
  inline val LSHARP      = 96;  push( LSHARP,      "<"             ) // these 2 are going to be hell because we need to be able to tell
  inline val RSHARP      = 97;  push( RSHARP,      ">"             ) // the difference between x.<(y) and x<y> -- one is a method, the other is a parameter
}

object Tokens extends Tokens
{
  inline val minToken = IDENTIFIER // Always 0
  def maxToken: Int = RSHARP // WIll change over versions
  val keywords: BitSet = BitSet(IF to MOD *)
}
