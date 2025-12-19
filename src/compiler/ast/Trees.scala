/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import psi.cc.utils.*
import psi.cc.ast.symbols.Flags.*
import scala.collection.*

trait Trees
{
  var nodeCount = 0

  case class Modifiers(flags: Int, privateWithin: Name) {
    def isPrivate   = !isPublic && !isInherited
    def isInherited = (flags & INHERITED) != 0
    def isVariable  = (flags & MUTABLE  ) != 0
    def isArgument  = (flags & PARAM    ) != 0
    def isAccessor  = (flags & ACCESSOR ) != 0
    def isOverride  = (flags & OVERRIDE ) != 0
    def isAbstract  = (flags & ABSTRACT ) != 0
    def isCase      = (flags & CASE     ) != 0
    def isSealed    = (flags & SEALED   ) != 0
    def isFinal     = (flags & FINAL    ) != 0
    def isTrait     = (flags & TRAIT    ) != 0
    def isPublic    = (falgs & PUBLIC   ) != 0
    def hasFlag (flag: Int) = (flags & flag) != 0
    def | (flag: Int): Modifiers =
      val f1 = flags | flag
      if (f1 == flags) this else Modifiers(f1, privateWithin)
  }

  def Modifiers (flags: Int ): Modifiers = Modifiers(flags, nme.EMPTY.toTypeName)
  def Modifiers (flags: Long): Modifiers = Modifiers(flags.toInt)
  val NoMods = Modifiers(0)

  abstract class Tree
  {
    nodeCount += 1

    private var posx: Int = Position.NOPOS
    def pos = posx

    var tpe: Type = _

    def setPos(p: Int): this.type = { posx = p; this }
    def setType(tp: Type): this.type = { tpe = tp; this }

    def symbol: Symbol = null
    def symbol_=(sym: Symbol): Unit
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }

    def hasSymbol = false
    def isDef = false
    def isTerm = false
    def isType = false
    def isEmpty = false

    //def hashCode: Int TODO: get Global (super).hashCode()

    def equals(that: Any): Boolean = that match
      case t: Tree => this eq t
      case _ => false

    def duplicate: this.type = (duplicator transform this).asInstanceOf[this.type]

    def copyAttrs(tree: Tree): this.type =
      posx = tree.posx
      tpe = tree.tpe
      if (hasSymbol) symbol = tree.symbol
      this
  }
}
