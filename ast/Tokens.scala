/*     ____  _____ ____                         *\
*    / __ \/ ___//  _/                         *
*   / /_/ /\__ \ / /       Psi-lang 2025       *
*  / ____/___/ // /                            *
* /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import ast.Token
import ast.TokenSet
import scala.collection.immutable.BitSet

object Tokens extends Tokens
abstract class Tokens
{
  def tokenRange(lo: Int, hi: Int): TokenSet = BitSet(lo to hi *)
  def tokenStr = new Array[String](maxToken + 1)
  def push(tk: Int, str: String): Unit = tokenStr(tk) = str

  def maxToken: Int = MOD                                         // NOTE: This is the highest token and will change
  def minToken: Int = EMPTY
  val keywords: TokenSet = BitSet(TRUE to MOD *)                  // NOTE: this is dynamically updated
  def isKeyword(x: Int): Boolean = keywords.contains(x)

  // AST utility
  inline val EMPTY        = 0;  push( EMPTY,      "!empty"      ) // Missing token, used in lookahead
  inline val EOF          = 1;  push( EOF,        "!eof"        ) // End of file
  inline val ERROR        = 2;  push( ERROR,      "!error"      ) // erroneous token

  // AST identifiers for token classes (Definitions aren't here)
  inline val APPLY        = 3;  push( APPLY,      "!apply"      ) // Apply a function
  inline val SELECT       = 4;  push( SELECT,     "!select"     ) // Select a value of a parent
  inline val EDITVAL      = 5;  push( EDITVAL,    "!editval"    ) // Edit a variable

  // Literals
  inline val CHARLIT      = 6;  push( CHARLIT,    "!char lit"   ) // ' '
  inline val INTLIT       = 7;  push( INTLIT,     "!int lit"    ) // 123
  inline val LONGLIT      = 8;  push( LONGLIT,    "!long lit"   ) // 123L or gt Int.max
  inline val DOUBLELIT    = 9;  push( DOUBLELIT,  "!double lit" ) // 123.456
  inline val FLOATLIT     = 10; push( FLOATLIT,   "!float lit"  ) // 123.456F
  inline val STRINGLIT    = 11; push( STRINGLIT,  "!string lit" ) // " "        AnyRef, so needs to be accessed from name table

  // Identifiers
  inline val IDENTIFIER   = 12; push( IDENTIFIER, "!identifier" ) // NameRef(x)
  inline val INTERPOLATION= 13; push( INTERPOLATION, "!str int" )

  // Symbols
  inline val DOT          = 14; push( DOT,        "."           ) // package accessor
  inline val COMMA        = 15; push( COMMA,      ","           ) // separate args
  inline val SEMI         = 16; push( SEMI,       ";"           ) // statement end (optional)
  inline val USCORE       = 17; push( USCORE,     "_"           ) // undefined value
  inline val TILDE        = 18; push( TILDE,      "~"           ) // all values
  inline val ASTERISK     = 19; push( ASTERISK,   "*"           ) // op pointer
  inline val AMPERSAND    = 19; push( AMPERSAND,  "&"           ) // op reference
  inline val EQUALS       = 20; push( EQUALS,     "="           ) // set a value to another
  inline val COLON        = 21; push( COLON,      ":"           ) // set a value's type to something
  inline val COLONS       = 22; push( COLONS,     "::"          ) // explicit module accessor
  inline val LARROW       = 23; push( LARROW,     "<-"          ) // i forgot lol
  inline val RARROW       = 24; push( RARROW,     "=>"          ) // lambda creation
  
  // Bracing
  inline val LPAREN       = 30; push( LPAREN,     "("           ) // Values
  inline val RPAREN       = 34; push( RPAREN,     ")"           )
  inline val LBRACKET     = 31; push( LBRACKET,   "["           ) // Types
  inline val RBRACKET     = 35; push( RBRACKET,   "]"           )
  inline val LSHARP       = 32; push( LSHARP,     "<"           ) // op-only parameters
  inline val RSHARP       = 36; push( RSHARP,     ">"           )
  inline val LBRACE       = 33; push( LBRACE,     "{"           ) // block
  inline val RBRACE       = 37; push( RBRACE,     "}"           )

  // Keywords
  inline val TRUE         = 50; push( TRUE,       "true"        ) //
  inline val FALSE        = 51; push( FALSE,      "false"       ) //
  inline val NULL         = 52; push( NULL,       "null"        ) //
  inline val IF           = 53; push( IF,         "if"          ) //
  inline val ELSE         = 54; push( ELSE,       "else"        ) //
  inline val WHILE        = 55; push( WHILE,      "while"       ) //
  inline val NEW          = 56; push( NEW,        "new"         ) //
  inline val THIS         = 57; push( THIS,       "this"        ) //
  inline val SUPER        = 58; push( SUPER,      "super"       ) //
  inline val MATCH        = 59; push( MATCH,      "match"       ) //
  inline val CASE         = 60; push( CASE,       "case"        ) //
  inline val EXTENDS      = 61; push( EXTENDS,    "extends"     ) //
  inline val TRY          = 62; push( TRY,        "try"         ) //
  inline val CATCH        = 63; push( CATCH,      "catch"       ) //
  inline val FINALLY      = 64; push( FINALLY,    "finally"     ) //
  inline val RETURN       = 65; push( RETURN,     "return"      ) //
  inline val THROW        = 66; push( THROW,      "throw"       ) //
  inline val IMPORT       = 67; push( IMPORT,     "import"      ) //

  // Modifiers
  inline val ABSTRACT     = 68; push( ABSTRACT,   "abstract"    ) //
  inline val FINAL        = 69; push( FINAL,      "final"       ) //
  inline val PRIVATE      = 70; push( PRIVATE,    "private"     ) //
  inline val INHERITED    = 71; push( INHERITED,  "inherited"   ) // protected in other languages
  inline val OVERRIDE     = 72; push( OVERRIDE,   "override"    ) //
  inline val SEALED       = 73; push( SEALED,     "sealed"      ) //

  // Data types
  inline val VAL          = 90; push( VAL,        "val"         ) // immutable value    -- VALDEF
  inline val VAR          = 91; push( VAR,        "var"         ) // mutable variable   -- part of VALDEF
  inline val LET          = 92; push( LET,        "let"         ) // QuBits             -- LETDEF
  inline val PRO          = 93; push( PRO,        "pro"         ) // Program entrypoint -- part of FNDEF
  inline val SUB          = 94; push( SUB,        "sub"         ) // Subroutine (ran with new) -- part of FNDEF
  inline val CO           = 95; push( CO,         "co"          ) // Coroutine          -- part of FNDEF
  inline val FN           = 96; push( FN,         "fn"          ) // Function           -- FNDEF
  inline val OP           = 97; push( OP,         "op"          ) // Operation          -- OPDEF
  inline val PACKAGE      = 98; push( PACKAGE,    "package"     ) // Package declare    -- PACKAGEDEF
  inline val OBJ          = 99; push( OBJ,        "obj"         ) // Object declare     -- OBJDEF
  inline val CLASS        = 100; push( CLASS,     "class"       ) // Class declare      -- CLASSDEF
  inline val TRAIT        = 101; push( TRAIT,     "trait"       ) // Trait declare      -- part of CLASSDEF
  inline val TYPE         = 102; push( TYPE,      "type"        ) // Type declare       -- might be TYPEDEF
  inline val MOD          = 103; push( MOD,       "mod"         ) // Module declare     -- MODDEF

  final val closingParens = BitSet(RPAREN, RBRACKET, RSHARP, RBRACE)
  final val openingParens = BitSet(LPAREN, LBRACKET, LSHARP, LBRACE)

}
