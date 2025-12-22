/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

import scala.util.control.Breaks.*
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.channels.FileChannel
import java.nio.channels.ReadableByteChannel
import java.nio.charset.CharsetDecoder
import java.nio.charset.CoderResult

class SourceReader(private val decoder: CharsetDecoder)
{
  private var chars = CharBuffer.allocate(0x4000)
  private var bytes = ByteBuffer.allocate(0x4000)


  def read(path: String): Array[Char] = read(new File(path))
  def read(file: File): Array[Char] =
    val ch = new FileInputStream(file).getChannel()
    try read(ch)
    finally ch.close()
  def read(file: AbstractFile): Array[Char] =
    val dec = this.decoder.reset()
    var bs  = this.bytes; bs.clear()
    var chs = this.chars; chs.clear()
    terminate(flush(dec, decode(dec, bs, chars, true)))
  def read(in: FileChannel): Array[Char] =
    val dec = this.decoder.reset()
    var bs  = ByteBuffer.allocate(0x4000)
    var chs = this.chars; chs.clear()
    var eof = false
    while (!eof) {
      eof = in.read(bs) < 0
      bs.flip()
      chs = decode(dec, bs, chs, eof)
    }
    terminate(flush(dec, chs))



  private def terminate(chs: CharBuffer): Array[Char] =
    val res = new Array[Char](chs.length)
    chs.get(res)
    this.chars = chs
    res

  private def decode(dec: CharsetDecoder, bs: ByteBuffer, chs: CharBuffer, eof: Boolean): CharBuffer =
    var chars = chs
    breakable:
      while true do
        var res = dec.decode(bs, chars, eof)
        if res.isUnderflow() then break
        if res.isError() then throw new IOException(res.toString())
        assert(res.isOverflow())
        chars = increaseCapacity(chars)
    bs.compact()
    chars


  private def flush(dec: CharsetDecoder, chs: CharBuffer): CharBuffer =
    var chars = chs
    breakable:
      while true do
        var res = dec.flush(chars)
        if res.isUnderflow() then break
        if res.isError() then throw new IOException(res.toString())
        assert(res.isOverflow())
        chars = increaseCapacity(chars)
    chars.flip()
    chars


  private def increaseCapacity(chs: CharBuffer): CharBuffer =
    chs.flip()
    val cap = 2 * chs.capacity
    CharBuffer.allocate(cap).put(chs)
}
