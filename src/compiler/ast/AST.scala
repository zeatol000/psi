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
  inline val magic = 0x5175CAFE  // QuCAFE -- reference to *.class' CAFEBABE & *.tasty's CAFEDADA

  var nTbl: NameTable = null
  inline infix def ++ (nt: NameTable): Unit = nTbl = nt

  
  //var untpd = mutable.Map[String, Untpd]()
  //var tpd   = mutable.Map[String, Tpd]()

  //infix def ++ (u: Untpd): Unit = untpd += (u.srcpath -> u)
  //infix def ++ (t: Tpd)  : Unit = tpd   += (t.srcpath -> t)

  // Temp functions to make scalac behave for a bit ///////////////////////////
}

