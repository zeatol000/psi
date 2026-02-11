/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package collection

object SixteenNibbles {
  inline val Width = 4
  inline val Mask = (1 << Width) - 1
  final val LongMask: Long = Mask.toLong
}

class SixteenNibbles(val bits: Long) extends AnyVal {
  import SixteenNibbles.*

  def apply(idx: Int): Int =
    (bits >>> (idx * Width)).toInt & Mask

  def updated(idx: Int, value: Int): SixteenNibbles =
    new SixteenNibbles(
      (bits & ~(LongMask << (idx * Width))) | ((value & Mask).toLong << (idx * Width))
    )

  def elements: IndexedSeq[Int] = 0 until 16 map apply

  override def toString: String =
    s"SixteenNibbles(${elements.toString})"
}
