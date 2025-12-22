/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast.symbols

import psi.cc.utils.NameTransform

class Names
{
  private val HASH_SIZE = 0x8000
  private val HASH_MASK = 0x7FFF
  private val NAME_SIZE = 0x20000

  final val nameDebug = false

  var chrs: Array[Char] = new Array(NAME_SIZE)
  private var nc = 0

  private val termHashtable, typeHashtable = new Array[Name](HASH_SIZE)



  def newTermName(cs: Array[Char], offset: Int, len: Int): Name =
    val h = hashValue(cs, offset, len) & HASH_MASK
    var n = termHashtable(h)
    while (
      n != null && (
        n.length != len ||
        !equals(n.start, cs, offset, len)
      )
    ) n = n.next
    if n == null then
      n = new TermName(nc, len, h)
      enterChars(cs, offset, len)
    n
  def newTermName(s: String): Name = newTermName(s.toCharArray, 0, s.length)
  def newTypeName(s: String): Name = newTermName(s).toTypeName
  def newTypeName(cs: Array[Char], offset: Int, len: Int): Name = newTermName(cs, offset, len).toTypeName

  def nameChars: Array[Char] = chrs



  implicit def view(s: String): Name = newTermName(s)



  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int = 
    if len > 0 then (
      len * (41 * 41 * 41) +
      cs(offset) * (41 * 41) +
      cs(offset + len - 1) * 41 +
      cs(offset + (len >> 1))
    ) else 0

  private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean =
    var i = 0
    while (
      i < len &&
      chrs (index + 1) == cs(offset + i)
    ) i += 1
    i == len

  private def enterChars(cs: Array[Char], offset: Int, len: Int): Unit =
    var i = 0
    while i < len do
      if nc + 1 == chrs.length then {
        val nchrs = new Array[Char](chrs.length * 2)
        System.arraycopy(chrs, 0, nchrs, 0, chrs.length)
        chrs = nchrs
      }
      chrs(nc + 1) = cs(offset + i)
      i += 1

    if (len == 0) nc = nc + 1
    else nc = nc + len





  abstract class Name(index: Int, len: Int) extends Function1[Int, Char]
  {
    var next: Name = null

    def start: Int = index
    final def length: Int = len
    final def isEmpty: Boolean = length == 0

    def isTermName: Boolean
    def isTypeName: Boolean
    def toTermName: Name
    def toTypeName: Name

    final def copyChars(cs: Array[Char], offset: Int) =
      System.arraycopy(chrs, index, cs, offset, len)

    final def toChars =
      val cs = new Array[Char](len)
      copyChars(cs, 0)
      cs

    final override def toString(): String = new String(chrs, index, len)
    final override def hashCode(): Int = index
    final def apply(i: Int): Char = chrs(index + i)

    final def pos(c: Char): Int = pos(c, 0)
    final def pos(s: String): Int = pos(s, 0)
    final def pos(c: Char, start: Int): Int =
      var i = start
      while (i < len && chrs(index + 1) != c) i += 1
      i
    final def pos(s: String, start: Int): Int =
      var i = pos(s.charAt(0), start)
      while (i + s.length <= len) do
        var j = 1
        while (s.charAt(j) == chrs(index + i + j)) do
          j += 1
          if (j == s.length()) return i
        i = pos(s.charAt(0), i + 1)
      len


    final def lastPos(c: Char): Int = lastPos(c, len - 1)
    final def lastPos(s: String): Int = lastPos(s, len - s.length())
    final def lastPos(c: Char, start: Int): Int =
      var i = start
      while (i >= 0 && chrs(index + i) != c) i = i - 1
      i
    final def lastPos(s: String, start: Int): Int =
      var i = lastPos(s.charAt(0), start)
      while (i >= 0) {
        var j = 1
        while (s.charAt(j) == chrs(index + i + j)) do
          j += 1
          if (j == s.length()) return i
        i = lastPos(s.charAt(0), i - 1)
      }
      -s.length()
    

    final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)
    final def startsWith(prefix: Name, start: Int): Boolean =
      var i = 0
      while (
        i < prefix.length &&
        start + i < len &&
        chrs(index + start + i) == chrs(prefix.start + i)
      ) i += 1
      i == prefix.length


    final def endsWith(suffix: Name): Boolean = endsWith(suffix, len)
    final def endsWith(suffix: Name, `end`: Int): Boolean =
      var i = 1
      while (
        i <= suffix.length && 
        i <= `end` &&
        chrs(index + `end` - i) == chrs(suffix.start + suffix.length - i)
      ) i += 1
      i > suffix.length


    def replace(from: Char, to: Char): Name =
      val cs = new Array[Char](len)
      var i = 0
      while (i < len) do
        val ch = this(i)
        cs(i) = if (ch == from) to else ch
        i += 1
      newTermName(cs, 0, len)

    def encode: Name =
      var str = toString()
      val res = NameTransform.encode(str)
      if (res == str) this
      else if (isTypeName) newTypeName(res)
      else newTermName(res)

    def decode: String =
      NameTransform.decode(toString())
  }



  private class TermName(index: Int, len: Int, hash: Int) extends Name(index, len)
  {
    next = termHashtable(hash)
    termHashtable(hash) = this
    def isTermName: Boolean = true
    def isTypeName: Boolean = false
    def toTermName: Name = this
    def toTypeName =
      val h = hashValue(chrs, index, len) & HASH_MASK
      var n = typeHashtable(h)
      while (n != null && n.start != index) n = n.next
      if (n == null) n = new TypeName(index, len, h)
      n
    def subName(from: Int, to: Int): Name =
      newTermName(chrs, start + from, to - from)
  }
  


  private class TypeName(index: Int, len: Int, hash: Int) extends Name(index, len)
  {
    next = typeHashtable(hash)
    typeHashtable(hash) = this
    def isTermName: Boolean = false
    def isTypeName: Boolean = true
    def toTypeName: Name = this
    def toTermName =
      val h = hashValue(chrs, index, len) & HASH_MASK
      var n = termHashtable(h)
      while (n != null && n.start != index) n = n.next
      if (n == null) n = new TermName(index, len, h)
      n
    def subName(from: Int, to: Int): Name =
      newTypeName(chrs, start + from, to - from)
  }
}
