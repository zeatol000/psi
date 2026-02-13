/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.parsing

//import psic.tools.comp.core.Decorators.*
//import psic.tools.comp.core.StdNames.nme

import scala.collection.immutable.BitSet
import scala.language.unsafeNulls

abstract class TokensCommon {
  /* special tokens */
  inline val EMPTY = 0;
  inline val ERROR = 1;
  inline val EOF = 2;

  /* literals */
  inline val CHARLIT = 3;
  inline val INTLIT = 4;
  inline val LONGLIT = 5;
  inline val FLOATLIT = 6;
  inline val DOUBLELIT = 7;
  inline val STRINGLIT = 8;
  inline val STRINGPART = 9;

  /* identifiers */
  inline val IDENTIFIER = 14;
  
  /* keywords */
  inline val IF        = 20;
  inline val ELSE      = 21;
  inline val WHILE     = 22;
  inline val TRY       = 23;
  inline val CATCH     = 24;
  inline val FINALLY   = 25;
  inline val MATCH     = 26;
  inline val CASE      = 27;
  inline val THROW     = 28;
  inline val RET       = 29;

  inline val TRUE      = 30;
  inline val FALSE     = 31;
  inline val THIS      = 32;
  inline val SUPER     = 33;
  inline val NULL      = 34;
  inline val NEW       = 35;

  inline val PKG       = 36;
  inline val USE       = 37;
  
  inline val VAL       = 38;
  inline val VAR       = 39;
  inline val LET       = 40;
  inline val FN        = 41;
  inline val OP        = 42;
  inline val CLS       = 43;
  inline val OBJ       = 44;
  inline val STRUCT    = 45;
  inline val ENUM      = 46;
  inline val TYPE      = 47;

  inline val OVR       = 48;
  inline val ABS       = 49;
  inline val FIN       = 50;
  inline val PUB       = 51;
  inline val PRO       = 52;
  inline val SEAL      = 53;

  /* special stuff */
  inline val COMMA     = 70;
  inline val SEMI      = 71;
  inline val DOT       = 72;
  inline val WILDCARD  = 73;
  inline val QMARK     = 74;
  inline val BANG      = 75;

  inline val EQUALS    = 76;
  inline val LARROW    = 77;
  inline val ARROW     = 78;
  inline val SUBTYPE   = 79;
  inline val SUPERTYPE = 80;
  inline val HASH      = 81;

  /* parentheses */
  inline val LPAREN    = 91;
  inline val RPAREN    = 92;
  inline val LBRACKET  = 93;
  inline val RBRACKET  = 94;
  inline val LBRACE    = 95;
  inline val RBRACE    = 96;
  inline val LSHARP    = 97;
  inline val RSHARP    = 98;
}
