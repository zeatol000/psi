/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io


import scala.language.unsafeNulls
import java.nio.file.{Files, Paths}

object Directory {
  import scala.util.Properties.userDir

  def Current: Option[Directory] =
    if (userDir == "") None
    else Some(apply(userDir).normalize)

  def inTempDirectory[T](fn: Directory => T): T = {
    val temp = Directory(Files.createTempDirectory("temp"))
    try fn(temp)
    finally temp.deleteRecursively()
  }

  def apply(path: String): Directory = apply(Paths.get(path))
  def apply(path: JPath): Directory = new Directory(path)
}

class Directory(jpath: JPath) extends Path(jpath) {
  override def toAbsolute: Directory = if (isAbsolute) this else super.toAbsolute.toDirectory
  override def toDirectory: Directory = this
  override def toFile: File = new File(jpath)
  override def normalize: Directory = super.normalize.toDirectory

  def list: Iterator[Path] =
    if (isDirectory) {
      val fileStream = Files.list(jpath)
      val files = fileStream.toArray(size => new Array[JPath](size))
      fileStream.close()
      files.iterator.map(Path.apply)
    }
    else Iterator.empty

  def dirs: Iterator[Directory] = list collect { case x: Directory => x }
  def files: Iterator[File] = list collect { case x: File => x }

  override def walkFilter(cond: Path => Boolean): Iterator[Path] =
    list.filter(cond).flatMap(_.walkFilter(cond))

  def deepFiles: Iterator[File] = Path.onlyFiles(deepList())

  def deepList(depth: Int = -1): Iterator[Path] =
    if (depth < 0) list ++ dirs.flatMap(_.deepList(depth))
    else if (depth == 0) Iterator.empty
    else list ++ dirs.flatMap(_.deepList(depth - 1))
}
