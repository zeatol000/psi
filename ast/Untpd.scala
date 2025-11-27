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
import scala.collection.*
import scala.util.control.Breaks.{break, breakable}

class Untpd
(
  val srcpath: String
)
{
  /** The Untpd class manages 2 things:
   *  the name table and ast tree data
   *  Using the following code snippet as an example:
   *
   * * * * *
   *
   *  pro main(): Null = {
   *    val x: String = "Hello, World!"
   *    println(x)
   *  }
   *
   * * * * *
   *
   *  Name table:
   *    00 00 00 06     // 6 values
   *    04 "main"       // the byte is the length of the name
   *    04 "Null"
   *    01 "x"
   *    06 "String"
   *    0D "Hello, World!"
   *    07 "println"
   *
   *  AST Data:
   *    34              // PROdef tag
   *    00              // no modifiers
   *    00 00 00 01     // name = NameTbl(0) = "main"
   *    01              // 1 parameter set
   *    01              // parentheses
   *    00              // No parameters
   *    00 00 00 02     // Exit type = NameTbl(1) = "Null"
   *    00 00 00 02     // token count; start program body
   *
   *      32            // VALdef tag
   *      00            // no modifiers
   *      00 00 00 03   // name = NameTbl(3) = "x"
   *      00 00 00 04   // Type = NameTbl(4) = "String"
   *      0F            // STRINGLIT tag
   *      00 00 00 05   // point to NameTbl(5)
   *            
   *      04            // APPLY tag
   *      00 00 00 05   // Apply function NameTbl(6) = "println"
   *      01            // 1 Argument
   *      00 00 00 03   // Arg$1 = NameTbl(3) = "x"
   *
   *  It gets a lot more complicated in certain situations..
   *  For example, if a value doesn't have a type explicitly assigned, then
   *  the type pointer will have to point to 0 (null). 
   *
   *  We can safely store values in files by doing this
   *  (lets say that x is 254, which is -2: Byte )
   *  file :+ x.toByte    // write 0xFE
   *  file.last & 0xFF    // read  0xFE -- returns Int, so very memory intensive
   *  
   * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

  /** Name table */

  var identifiers = mutable.Seq[Seq[Byte]](Seq(0))  // occupy index 0
  inline def idCount: Int = identifiers.length
  inline infix def lengthOf (x: Int): Int = identifiers(x).length

  inline infix def addId (x: Seq[Byte])(using td: TD): Unit =
    if !identifiers.contains(x) then identifiers = identifiers :+ x

  inline infix def indexOf (x: Seq[Byte]): Int = identifiers.indexOf(x)

  def dumpId: String =
    val a: Seq[String] = identifiers.map(_.map(_.toChar).mkString(""))
    return a.mkString("\n")



  /** AST table */
  def dumpAst: String =
    (nestBuf.filter(_ != null).map { t => t match {

      case v: VALDEF    => s"""VALDEF
    ${v.modifiers.map(tokenStr(_)).mkString(", ")}
    ${identifiers(v.name).map(_.toChar).mkString("")}
    ${identifiers(v.valType).map(_.toChar).mkString("")}
    ${v.value}         """

      case f: FNDEF     => s"""FNDEF
    ${f.modifiers.map(tokenStr(_)).mkString(", ")}
    ${identifiers(f.name).map(_.toChar).mkString("")}
    ${f.paramGet}
    ${identifiers(f.valType).map(_.toChar).mkString("")}
    ${f.body.length}   """

      case o: OPDEF     => s"""OPDEF
    ${o.modifiers.map(tokenStr(_)).mkString(", ")}
    ${identifiers(o.name).map(_.toChar).mkString("")}
    ${o.paramGet}
    ${identifiers(o.valType).map(_.toChar).mkString("")}
    ${o.body.length}   """

      case c: CLASSDEF  => s"""CLASSDEF
    ${c.modifiers.map(tokenStr(_)).mkString(", ")}
    ${identifiers(c.name).map(_.toChar).mkString("")}
    ${c.body.length}   """

      case o: OBJDEF    => s"""OBJDEF
    ${o.modifiers.map(tokenStr(_)).mkString(", ")}
    ${identifiers(o.name).map(_.toChar).mkString("")}
    ${o.body.length}   """

      case a: APPLY     => s"""APPLY
    ${identifiers(a.name).map(_.toChar).mkString("")}
    ${a.paramGet}      """

      case s: SELECT    => s"""SELECT
    ${identifiers(s.obj).map(_.toChar).mkString("")} """

      case null         => s"""NULLREF ERROR"""
    }}).mkString("\n")

  var opBuf: untpdToken = null
  var nestBuf           = mutable.Seq[untpdToken]()
  var paramBuf: pBuf    = null

  // opBuf
  infix def ! (x: untpdToken): Unit = 
    this.<<(x)
    var i = nestBuf.length - 1
    breakable { while (i >= 0) {
      nestBuf(i) match {
        case f: FNDEF    if f.open => f.body = f.body :+ opBuf; break
        case o: OPDEF    if o.open => o.body = o.body :+ opBuf; break
        case c: CLASSDEF if c.open => c.body = c.body :+ opBuf; break
        case o: OBJDEF   if o.open => o.body = o.body :+ opBuf; break
        case _ => i = i - 1
      }
    }}
    
  infix def <> (x: pBuf): Unit = paramBuf = x
  infix def operate (x: untpdToken): Unit =
    if (opBuf != null) this.!(opBuf)
    opBuf = x

  infix def << (x: untpdToken): Unit =
    nestBuf = nestBuf :+ opBuf
    nestBuf = nestBuf.distinct
    opBuf = null

  def canParam : Boolean = opBuf match {
    case f: FNDEF => true
    case o: OPDEF => true
    case c: CLASSDEF => true
    case a: APPLY => true
    case _ => false
  }
  
}
///////////////////////////////////////////////////////////////////////////////

