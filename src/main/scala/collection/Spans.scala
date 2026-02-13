/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import scala.language.implicitConversions

object Spans {

  private inline val StartEndBits = 26
  private inline val StartEndMask = (1L << StartEndBits) - 1
  private inline val SyntheticPointDelta = (1 << (64 - StartEndBits * 2)) - 1

  final val MaxOffset = StartEndMask.toInt

  class Span( val coords: Long ) extends AnyVal {
    def exists: Boolean = this != NoSpan

    def start: Int = {
      assert(exists, "start of NoSpan")
      (coords & StartEndMask).toInt
    }

    def end: Int = {
      assert(exists, "end of NoSpan")
      ((coords >>> StartEndBits) & StartEndMask).toInt
    }

    def point: Int = {
      assert(exists, "point of NoSpan")
      val poff = pointDelta
      if (poff == SyntheticPointDelta) start else start + poff
    }

    def pointMayBeIncorrect =
      pointDelta == 0 && end - start >= SyntheticPointDelta

    def pointDelta: Int =
      (coords >>> (StartEndBits * 2)).toInt

    def orElse(that: Span): Span =
      if (this.exists) this else that

    def union(that: Span): Span =
      if (!this.exists) that
      else if (!that.exists) this
      else Span(this.start min that.start, this.end max that.end, this.point)

    def contains(that: Span): Boolean =
      !that.exists || exists && (start <= that.start && end >= that.end)

    def overlaps(that: Span): Boolean = {
      def containsInner(span: Span, offset: Int) = span.start < offset && offset < span.end
      exists && that.exists && (
         containsInner(this, that.start)
      || containsInner(this, that.end)
      || containsInner(that, this.start)
      || containsInner(that, this.end)
      )
    }

    def isSynthetic: Boolean = pointDelta == SyntheticPointDelta
    def isSourceDerived: Boolean = !isSynthetic
    def isZeroExtent: Boolean = exists && start == end

    def shift(offset: Int): Span =
      if (exists) fromOffsets(start + offset, end + offset, pointDelta)
      else this

    def focus: Span = if (exists) Span(point) else NoSpan

    def startPos: Span = if (exists) Span(start) else NoSpan
    def endPos: Span = if (exists) Span(end) else NoSpan

    def withStart(start: Int): Span =
      if (exists) fromOffsets(start, this.end, if (isSynthetic) SyntheticPointDelta else this.point - start)
      else this

    def withEnd(end: Int): Span =
      if (exists) fromOffsets(this.start, end, pointDelta)
      else this

    def withPoint(point: Int): Span =
      if (exists) fromOffsets(this.start, this.end, point - this.start)
      else this

    def toSynthetic: Span = if (isSynthetic) this else Span(start, end)

    override def toString: String = {
      val (left, right) = if (isSynthetic) ("<", ">") else ("[", "]")
      if (exists)
        s"$left$start..${if (point == start) "" else s"$point.."}$end$right"
      else
        s"${left}no position${right}"
    }

    def ==(that: Span): Boolean = this.coords == that.coords
    def !=(that: Span): Boolean = this.coords != that.coords
  }

  private def fromOffsets(start: Int, end: Int, pointDelta: Int) =
    new Span(
      (start & StartEndMask).toLong |
      ((end & StartEndMask).toLong << StartEndBits) |
      (pointDelta.toLong << (StartEndBits * 2)))

  def Span(start: Int, end: Int): Span =
    fromOffsets(start, end, SyntheticPointDelta)

  def Span(start: Int, end: Int, point: Int): Span = {
    val pointDelta = (point - start) max 0
    fromOffsets(start, end, if (pointDelta >= SyntheticPointDelta) 0 else pointDelta)
  }

  def Span(start: Int): Span = Span(start, start)

  val NoSpan: Span = Span(1, 0)

  class Coord(val encoding: Int) extends AnyVal {
    def isIndex: Boolean = encoding > 0
    def isSpan: Boolean = encoding <= 0
    def toIndex: Int = {
      assert(isIndex)
      encoding - 1
    }
    def toSpan: Span = {
      assert(isSpan)
      if (this == NoCoord) NoSpan else Span(-1 - encoding)
    }
    override def toString = if isSpan then s"$toSpan" else s"Coord(idx=$toIndex)"
  }

  implicit def indexCoord(n: Int): Coord = new Coord(n + 1)
  implicit def spanCoord(span: Span): Coord =
    if (span.exists) new Coord(-(span.point + 1))
    else NoCoord

  val NoCoord: Coord = new Coord(0)
}
