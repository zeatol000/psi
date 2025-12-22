/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

import java.io.{File, FileInputStream, IOException}
import java.util.Iterator
import java.util.NoSuchElementException

class PlainFile(
  private val file: File
) extends AbstractFile {

  assert( file != null )
  if (!file.exists()) throw new Exception(s"non-existent file: $file")


  def fromPath(path: String) = fromFile(new File(path))
  def fromFile(f:    File  ) = if f.exists() then PlainFile(f) else null

  def getName: String = file.getName()
  def getPath: String = file.getPath()
  def getFile: File = file

  def hashCode: Int = try
    file.getCanonicalPath().hashCode()
  catch
    case ioe: IOException => 0


  def equals(that: Any): Boolean = try
    that.isInstanceOf[PlainFile] &&
    file.getCanonicalPath().equals(that.asInstanceOf[PlainFile].file.getCanonicalPath())
  catch
    case ioe: IOException =>
      that.isInstanceOf[PlainFile] &&
      file.getAbsolutePath().equals(that.asInstanceOf[PlainFile].file.getAbsolutePath())


  def isDirectory: Boolean = file.isDirectory
  def lastModified: Long = file.lastModified

  def read: Array[Char] =
    assert(!isDirectory, "Cannot read directory "+this)
    val in: FileInputStream = new FileInputStream(file)
    var rest = file.length().asInstanceOf[Int]
    var buf = new Array[Byte](rest)
    while ({
      var res: Int = in.read(buf, buf.length - rest, rest)
      if (res == -1) throw new IOException("read error")
      rest -= res
      rest > 0
    }) ()
    in.close()
    buf.map{_.toChar}

  // def list???/

  def lookupName(name: String, dir: Boolean): AbstractFile =
    assert(isDirectory, "not a directory "+this)
    var child = new File(file, name)
    if !child.exists() then null
    else if isDirectory != child.isDirectory() then null
    else if isDirectory == child.isFile() then null
    else new PlainFile(child)
  

}
