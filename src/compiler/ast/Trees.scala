/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import psi.cc.utils.*
import scala.collection.*
import scala.annotation.internal.sharable, unchecked.uncheckedVariance
import compiletime.uninitialized

object Trees
{
  type Untyped = Type | Null

  @sharable var ntrees: Int = 0

  abstract class Tree
  [+T <: Untyped](Content: Seq[Char], Path: String)
  {
    ntrees += 1
    type ThisTree[T <: Untyped] <: Tree[T]

    protected var _tpe: T @uncheckedVariance = uninitialized

    def overwriteType(tpe: T @uncheckedVariance): Unit = _tpe = tpe

    final def tpe: T =
      if _tpe == null then throw UnAssignedTypeException(this)
      _type.uncheckedNN
  }
}
