/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import scala.collection.mutable

class NameTable {
  val HASHSIZE = 0x2000

  protected val table: Array[String] = new Array(HASHSIZE)

  def last: Int = table.lastIndexWhere(_ != null)
  def contains(x: String): Boolean = table.contains(x)
  def insert(x: String): Unit = if (!contains(x)) table(last) = x
  def indexOf(x: String): Int = table.indexOf(x)
}

type NameRef = Int

case class Name (name: String) extends NameTable {
  def index: Int = table.indexOf(name)
  def insert: Unit = insert(name)
}

case class NameType (name: String) extends NameTable {
  def index: Int = table.indexOf(name)
  def insert: Unit = insert(name)
}
