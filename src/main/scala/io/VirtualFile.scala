/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import scala.language.unsafeNulls

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream }

class VirtualFile(val name: String, override val path: String) extends AbstractFile {

  def this(name: String) = this(name, name)

  def this(path: String, content: Array[Byte]) = {
    this(VirtualFile.nameOf(path), path)
    this.content = content
  }

  def this(path: JPath, content: Array[Byte]) = {
    this(path.getFileName().toString(), path.toString())
    this.content = content
    this.jpath_ = path
  }

  private var content = Array.emptyByteArray

  private var jpath_ : JPath = null

  def absolute: AbstractFile = this

  def jpath: JPath = jpath_

  override def sizeOption: Option[Int] = Some(content.length)

  override def exists: Boolean = true

  def input : InputStream = new ByteArrayInputStream(content)

  override def output: OutputStream = {
    new ByteArrayOutputStream() {
      override def close() = {
        super.close()
        content = toByteArray()
      }
    }
  }

  def container: AbstractFile = NoAbstractFile

  def isDirectory: Boolean = false

  override def isVirtual: Boolean = true


  def lastModified: Long = 0

  def iterator: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    Iterator.empty
  }

  def lookupName(name: String, directory: Boolean): AbstractFile = {
    assert(isDirectory, "not a directory '" + this + "'")
    null
  }


  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()
}
object VirtualFile:
  private def nameOf(path: String): String =
    val i = path.lastIndexOf('/')
    if i >= 0 && i < path.length - 1 then path.substring(i + 1) else path
