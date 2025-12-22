/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

import java.util.ArrayList

abstract class AbstractTimer extends Timer
{
  val starts = new ArrayList[Long]()

  def issue(msg: String, duration: Long): Unit
  def start: Unit = starts.add(System.currentTimeMillis())
  def stop(msg: String): Unit =
    var stop: Long = System.currentTimeMillis()
    var start: Long = starts.remove(starts.size() - 1).asInstanceOf[Long]
    issue(msg, stop - start)

  def drop: Unit = starts.remove(starts.size() - 1)
}
