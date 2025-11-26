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
    (nestBuf.map { t => t match {
      case v: VALDEF    => s"""|VALDEF
        |${v.modifiers.mkString(", ")}
        |${v.name} (NameRef)
        |${v.valType} (NameRef)
        |${v.value}         """.stripMargin
      case f: FNDEF     => s"""|FNDEF
        |${f.modifiers.mkString(", ")}
        |${f.name} (NameRef)
        |${f.valType} (NameRef)
        |${f.body.length}   """.stripMargin
      case o: OPDEF     => s"""|OPDEF
        |${o.modifiers.mkString(", ")}
        |${o.name} (NameRef)
        |${o.valType} (NameRef)
        |${o.body.length}   """.stripMargin
      case c: CLASSDEF  => s"""|CLASSDEF
        |${c.modifiers.mkString(", ")}
        |${c.name} (NameRef)
        |${c.body.length}   """.stripMargin
      case o: OBJDEF    => s"""|OBJDEF
        |${o.modifiers.mkString(", ")}
        |${o.name} (NameRef)
        |${o.body.length}   """.stripMargin
      case a: APPLY     => s"""|APPLY
        |${a.name} (NameRef)
        |${a.params}        """.stripMargin
      case s: SELECT    => s"""
        |${s.obj} (NameRef) """.stripMargin
    }}).mkString("\n")
  var opBuf: untpdToken = null
  var nestBuf           = mutable.Seq[untpdToken]()
  var paramBuf: pBuf    = null

  // opBuf
  infix def <> (x: pBuf): Unit = paramBuf = x
  infix def operate (x: untpdToken): Unit =
    if (opBuf != null) this.<<(opBuf)  
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
class pBuf (            // Name,    Type,   value
  var paren: mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0, 0, null)),
  var brack: mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0, 0, null)),
  var sharp: mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0, 0, null)),
  )


type paramType = mutable.Seq[pBuf]
type parentType = mutable.Seq[paramType]
type NameRef = Int
abstract class untpdToken

class VALDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var valType: NameRef = 0,
  var value: Byte|NameRef = EMPTY,
  ) extends untpdToken



class FNDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var params: paramType = mutable.Seq(),
  var valType: NameRef = 0,
  var body: mutable.Seq[untpdToken] = mutable.Seq(),

  var open: Boolean = true,
  val startDepth: Byte,
  ) extends untpdToken



class OPDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var params: paramType = mutable.Seq(),
  var valType: NameRef = 0,
  var body: mutable.Seq[untpdToken] = mutable.Seq(),

  var open: Boolean = true,
  val startDepth: Byte,
  ) extends untpdToken



class CLASSDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var parents: parentType = mutable.Seq(),
  var body: mutable.Seq[untpdToken] = mutable.Seq(),

  var open: Boolean = true,
  val startDepth: Byte,
  ) extends untpdToken



class OBJDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var parents: parentType = mutable.Seq(),
  var body: mutable.Seq[untpdToken] = mutable.Seq(),

  var open: Boolean = true,
  val startDepth: Byte,
  ) extends untpdToken



class APPLY (
  var name: NameRef = 0,
  var params: paramType = mutable.Seq(),
  ) extends untpdToken



class SELECT (
  var obj: NameRef = 0,
  var member: untpdToken = null,
  ) extends untpdToken


