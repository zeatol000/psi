/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package collection

class CharBuffer(initialSize: Int = 1024)
{
  private var cs: Array[Char] = new Array[Char](initialSize)
  private var len: Int = 0

  inline def :+ (ch: Char): Unit = this.append(ch)

  def append(ch: Char): Unit =
    if (len == cs.length) {
      val cs1 = new Array[Char](len * 2)
      Array.copy(cs, 0, cs1, 0, len)
      cs = cs1
    }
    cs(len) = ch
    len += 1


  def chars: Array[Char] = cs
  def length: Int = len
  def isEmpty: Boolean = len == 0
  def last: Char = cs(len - 1)
  def clear(): Unit = len = 0

  override def toString = String(cs, 0, len)
}
