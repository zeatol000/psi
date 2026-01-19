/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import scala.language.unsafeNulls

import java.nio.file.{FileSystemAlreadyExistsException, FileSystems}

import scala.jdk.CollectionConverters.*

class JarArchive private (val jarPath: Path, root: Directory) extends PlainDirectory(root) {
  def close(): Unit = this.synchronized(jpath.getFileSystem().close())
  override def exists: Boolean = jpath.getFileSystem().isOpen() && super.exists
  def allFileNames(): Iterator[String] =
    java.nio.file.Files.walk(jpath).iterator().asScala.map(_.toString)

  override def toString: String = jarPath.toString
}

object JarArchive {
  def create(path: Path): JarArchive = {
    require(path.ext.isJar)
    path.delete()
    open(path, create = true)
  }

  /** Create a jar file. */
  def open(path: Path, create: Boolean = false): JarArchive = {
    require(path.ext.isJar)

    val env = Map("create" -> create.toString).asJava
    val uri = java.net.URI.create("jar:" + path.toAbsolute.toURI.toString)
    val fs = {
      try FileSystems.newFileSystem(uri, env)
      catch {
        case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri)
      }
    }
    val root = fs.getRootDirectories().iterator.next()
    new JarArchive(path, Directory(root))
  }
}
