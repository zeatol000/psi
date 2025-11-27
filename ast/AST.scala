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

class AST
{
  val magic = 0x5175CAFE  // QuCAFE -- reference to *.class' CAFEBABE & *.tasty's CAFEDADA

  var untpd = mutable.Map[String, Untpd]()
  var nTbl  = mutable.Map[String, mutable.Seq[Seq[Byte]]]()
  var uTags = mutable.Map[String, mutable.Seq[untpdToken]]()
  var tpd   = mutable.Map[String, Tpd]()

  infix def ++ (u: Untpd): Unit = untpd += (u.srcpath -> u)
  infix def ++ (t: Tpd)  : Unit = tpd   += (t.srcpath -> t)
  infix def !! (x: Byte) : Unit = if (x eq 0) untpd = mutable.Map() else if (x eq 1) tpd = mutable.Map()
  infix def << (u: Untpd): Unit =
    nTbl += (u.srcpath -> u.identifiers)
    var buf = mutable.Seq[untpdToken]()
    u.nestBuf foreach {(_: untpdToken) match {
      case v: VALDEF   => buf = buf :+ v
      case f: FNDEF    => f.params = mutable.Seq(mergeParams(f.params.toSeq *)); buf = buf :+ f 
      case o: OPDEF    => o.params = mutable.Seq(mergeParams(o.params.toSeq *)); buf = buf :+ o
      case c: CLASSDEF => buf = buf :+ c
      case o: OBJDEF   => buf = buf :+ o
      case a: APPLY    => a.params = mutable.Seq(mergeParams(a.params.toSeq *)); buf = buf :+ a
      case s: SELECT   => buf = buf :+ s
      case null        => void
    }}
    uTags += (u.srcpath -> buf)



  private def mergeParams(xs: pBuf*): pBuf = {
    //var paren, brack, sharp = mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0,0,null))
    val paren = mutable.Seq[(NameRef, NameRef, ?)]( xs.flatMap { p => p.paren} *).filterNot(_ == (0,0,null))
    val brack = mutable.Seq[(NameRef, NameRef, ?)]( xs.flatMap { p => p.brack} *).filterNot(_ == (0,0,null))
    val sharp = mutable.Seq[(NameRef, NameRef, ?)]( xs.flatMap { p => p.sharp} *).filterNot(_ == (0,0,null))

    new pBuf (
      if paren.length >= 1 then paren else mutable.Seq((0,0,null)),
      if brack.length >= 1 then brack else mutable.Seq((0,0,null)),
      if sharp.length >= 1 then sharp else mutable.Seq((0,0,null)),
      )
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
  ) extends untpdToken {
  def paramGet: String = 
    val p = params.filterNot(_ == null).map { pb => // RAM hates to see me coming 
      s"(${if (pb.paren(0) == (0,0,null)) "" else pb.paren.mkString(", ") })   " +
      s"[${if (pb.brack(0) == (0,0,null)) "" else pb.brack.mkString(", ") }]   " +
      s"<${if (pb.sharp(0) == (0,0,null)) "" else pb.brack.mkString(", ") }>"
    }
    p mkString "\n"
}



class OPDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var params: paramType = mutable.Seq(),
  var valType: NameRef = 0,
  var body: mutable.Seq[untpdToken] = mutable.Seq(),

  var open: Boolean = true,
  val startDepth: Byte,
  ) extends untpdToken {
  def paramGet: String = 
    val p = params.map { pb => // RAM hates to see me coming 
      s"(${if (pb.paren(0) == (0,0,null)) "" else pb.paren.mkString(", ") })   " +
      s"[${if (pb.brack(0) == (0,0,null)) "" else pb.brack.mkString(", ") }]   " +
      s"<${if (pb.sharp(0) == (0,0,null)) "" else pb.brack.mkString(", ") }>"
    }
    p mkString "\n"
}



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
  ) extends untpdToken {
  def paramGet: String = 
    val p = params.map { pb => // RAM hates to see me coming 
      s"(${if (pb.paren(0) == (0,0,null)) "" else pb.paren.mkString(", ") })   " +
      s"[${if (pb.brack(0) == (0,0,null)) "" else pb.brack.mkString(", ") }]   " +
      s"<${if (pb.sharp(0) == (0,0,null)) "" else pb.brack.mkString(", ") }>"
    }
    p mkString "\n"
}



class SELECT (
  var obj: NameRef = 0,
  var member: untpdToken = null,
  ) extends untpdToken


