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
  inline val FOR       = 23;
  inline val YIELD     = 24;
  inline val TRY       = 25;
  inline val CATCH     = 26;
  inline val FINALLY   = 27;
  inline val MATCH     = 28;
  inline val CASE      = 29;
  inline val THROW     = 30;
  inline val RET       = 31;

  inline val TRUE      = 32;
  inline val FALSE     = 33;
  inline val THIS      = 34;
  inline val SUPER     = 35;
  inline val NULL      = 36;
  inline val NEW       = 37;

  inline val PKG       = 38;
  inline val USE       = 39;
  
  inline val VAL       = 40;
  inline val VAR       = 41;
  inline val LET       = 42;
  inline val FN        = 43;
  inline val OP        = 44;
  inline val PGRM      = 45;
  inline val CLS       = 46;
  inline val OBJ       = 47;
  inline val STRUCT    = 48;
  inline val ENUM      = 49;
  inline val TYPE      = 50;

  inline val OVR       = 51;
  inline val ABS       = 52;
  inline val FIN       = 53;
  inline val PUB       = 54;
  inline val PRO       = 55;
  inline val SEAL      = 56;

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
