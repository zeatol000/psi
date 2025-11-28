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
      case i: If       =>
        if (i.ifBuf == mutable.Seq()) { i.ifBuf = i.body; i.body = mutable.Seq() }; buf = buf :+ i
      case null        => void
    }}
    uTags += (u.srcpath -> buf)



  private def mergeParams(xs: pCreate*): pCreate = {
    //var paren, brack, sharp = mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0,0,null))
    val paren = mutable.Seq[(NameRef, NameRef, ?)]( xs.flatMap { p => p.paren} *).filterNot(_ == (0,0,null))
    val brack = mutable.Seq[(NameRef, NameRef, ?)]( xs.flatMap { p => p.brack} *).filterNot(_ == (0,0,null))
    val sharp = mutable.Seq[(NameRef, NameRef, ?)]( xs.flatMap { p => p.sharp} *).filterNot(_ == (0,0,null))

    new pCreate (
      if paren.length >= 1 then paren else mutable.Seq((0,0,null)),
      if brack.length >= 1 then brack else mutable.Seq((0,0,null)),
      if sharp.length >= 1 then sharp else mutable.Seq((0,0,null)),
      )
  }
  private def mergeParams(xs: pApply*): pApply = {
    val p = mutable.Seq[(Byte, ?)]( xs.flatMap { p => p.paren} *).filterNot(_ == (1,0))
    val b = mutable.Seq[(Byte, ?)]( xs.flatMap { p => p.brack} *).filterNot(_ == (1,0))
    val s = mutable.Seq[(Byte, ?)]( xs.flatMap { p => p.sharp} *).filterNot(_ == (1,0))
    
    new pApply (
      if p.length >= 1 then p else mutable.Seq((1,0)),
      if b.length >= 1 then b else mutable.Seq((1,0)),
      if s.length >= 1 then s else mutable.Seq((1,0)),
      )
  }
}

