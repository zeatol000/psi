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

abstract class pBuf
class pCreate (            // Name,    Type,   value
  var paren: mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0, 0, null)),
  var brack: mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0, 0, null)),
  var sharp: mutable.Seq[(NameRef, NameRef, ?)] = mutable.Seq((0, 0, null)),
  ) extends pBuf
class pApply (    // a list of identifiers ig
  var paren: mutable.Seq[(Byte, ?)] = mutable.Seq((1,0)),
  var brack: mutable.Seq[(Byte, ?)] = mutable.Seq((1,0)),
  var sharp: mutable.Seq[(Byte, ?)] = mutable.Seq((1,0)),
  ) extends pBuf

type paramCreate = mutable.Seq[pCreate]
type paramApply = mutable.Seq[pApply]
type parentType = mutable.Seq[NameRef]
type NameRef = Int
abstract class untpdToken

class VALDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var valType: NameRef = 0,
  var value: (Byte, ?) = (EMPTY, 0),
  ) extends untpdToken



class FNDEF (
  var modifiers: mutable.Seq[Byte] = mutable.Seq(),
  var name: NameRef = 0,
  var params: paramCreate = mutable.Seq(),
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
  var params: paramCreate = mutable.Seq(),
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
  var params: paramApply = mutable.Seq(),
  ) extends untpdToken {
  def paramGet: String = 
    val p = params.map { pb => // RAM hates to see me coming 
      s"(${ if pb.paren(0) == (1,0) then "" else pb.paren.mkString(", ") })   " +
      s"[${ if pb.brack(0) == (1,0) then "" else pb.brack.mkString(", ") }]   " +
      s"<${ if pb.sharp(0) == (1,0) then "" else pb.sharp.mkString(", ") }>"
    }
    p mkString "\n"
}



class SELECT (
  var obj: NameRef = 0,
  var member: (Byte, ?) = (EMPTY, 0)
  ) extends untpdToken




class If (
  var cond: paramApply = mutable.Seq(),
  var ifBuf: mutable.Seq[untpdToken] = mutable.Seq(),

  var body: mutable.Seq[untpdToken] = mutable.Seq(),
  var open: Boolean = true,
  ) extends untpdToken {
  def getCond: String =
    val p = cond.map { pb => // RAM hates to see me coming 
      s"(${ if pb.paren(0) == (EMPTY,0) then "" else pb.paren.mkString(", ") })   " +
      s"[${ if pb.brack(0) == (EMPTY,0) then "" else pb.brack.mkString(", ") }]   " +
      s"<${ if pb.sharp(0) == (EMPTY,0) then "" else pb.sharp.mkString(", ") }>"
    }
    p mkString "\n"
}



class While (
  var cond: paramApply = mutable.Seq(),
  var body: mutable.Seq[untpdToken] = mutable.Seq(),
  var open: Boolean = true,
  ) extends untpdToken



class New (
  var name: NameRef = 0,
  var params: paramApply = mutable.Seq(),
  ) extends untpdToken



// TODO: This, Super, Match, Case, Try, Catch, Finally(?)

class Return (
  var value: (Byte, ?) = (EMPTY, 0),
  ) extends untpdToken

// TODO: Throw

class Import (
  var names: mutable.Seq[Seq[Byte]] = mutable.Seq()
  ) extends untpdToken

class PACKAGEDEF (
  var names: mutable.Seq[Seq[Byte]] = mutable.Seq()
  ) extends untpdToken
