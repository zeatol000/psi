/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import scala.language.unsafeNulls

import scala.collection.mutable
import java.io.{InputStream, OutputStream}



class VirtualDirectory(val name: String, maybeContainer: Option[VirtualDirectory] = None)
extends AbstractFile {
  def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path + '/' + name
    }

  def absolute: AbstractFile = this

  def container: VirtualDirectory = maybeContainer.get
  def isDirectory: Boolean = true
  override def isVirtual: Boolean = true
  val lastModified: Long = System.currentTimeMillis

  override def jpath: JPath = null
  override def input: InputStream = sys.error("directories cannot be read")
  override def output: OutputStream = sys.error("directories cannot be written")

  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

  private val files = mutable.Map.empty[String, AbstractFile]

  def iterator: Iterator[AbstractFile] = files.values.toList.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile =
    (files get name filter (_.isDirectory == directory)).orNull

  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = false)) getOrElse {
      val newFile = new VirtualFile(name, s"$path/$name")
      files(name) = newFile
      newFile
    }

  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = true)) getOrElse {
      val dir = new VirtualDirectory(name, Some(this))
      files(name) = dir
      dir
    }

  def clear(): Unit = {
    files.clear()
  }
}
