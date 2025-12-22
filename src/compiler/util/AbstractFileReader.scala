/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

import java.io.IOException
import scala.compiletime.uninitialized

class AbstractFileReader(
  val file: AbstractFile
) {
  var buf: Array[Char] = file.read
  var bp: Int = 0

  def charAt(pos: Int): Char = buf(pos)
  def nextChar(): Char = { bp += 1; buf(bp) }

  def nextChars(len: Int): Array[Char] =
    var res = new Array[Char](len)
    System.arraycopy(buf, bp, res, 0, len)
    bp += len
    res

  def skip(n: Int) = bp += n
}

