/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import scala.language.unsafeNulls

import java.io.{InputStream, OutputStream}
import java.nio.file.{InvalidPathException, Paths}


object PlainFile {
  extension (jp: JPath)
    def toPlainFile = new PlainFile(new Path(jp))
}

class PlainDirectory(givenPath: Directory) extends PlainFile(givenPath) {
  override val isDirectory = true
  override def iterator: Iterator[PlainFile] = givenPath.list
    .filter(_.exists).map(new PlainFile(_))  
}



class PlainFile(val givenPath: Path) extends AbstractFile {
  assert(path != null)
  //comp.util.Stats.record("new PlainFile")


  def jpath: JPath = givenPath.jpath

  override def underlyingSource  = {
    val fileSystem = jpath.getFileSystem
    fileSystem.provider().getScheme match {
      case "jar" =>
        val fileStores = fileSystem.getFileStores.iterator()
        if (fileStores.hasNext) {
          val jarPath = fileStores.next().name
          try {
            Some(new PlainFile(new Path(Paths.get(jarPath.stripSuffix(fileSystem.getSeparator)))))
          } catch {
            case _: InvalidPathException =>
              None
          }
        } else None
      case "jrt" =>
        if (jpath.getNameCount > 2 && jpath.startsWith("/modules")) {
          // TODO limit this to OpenJDK based JVMs?
          val moduleName = jpath.getName(1)
          Some(new PlainFile(new Path(Paths.get(System.getProperty("java.home"), "jmods", moduleName.toString + ".jmod"))))
        } else None
      case _ => None
    }
  }


  def name: String = givenPath.name

  def path: String = givenPath.path

  override val absolutePath: String = givenPath.toAbsolute.toString.intern

  def absolute: PlainFile = new PlainFile(givenPath.toAbsolute)

  override def container: AbstractFile = new PlainFile(givenPath.parent)
  override def input: InputStream = givenPath.toFile.inputStream
  override def output: OutputStream = givenPath.toFile.outputStream()
  override def sizeOption: Option[Int] = Some(givenPath.length.toInt)

  override def hashCode(): Int = System.identityHashCode(absolutePath)
  override def equals(that: Any): Boolean = that match {
    case x: PlainFile => absolutePath `eq` x.absolutePath
    case _            => false
  }

  val isDirectory: Boolean = givenPath.isDirectory // cached for performance on Windows

  def lastModified: Long = givenPath.lastModified.toMillis

  def iterator: Iterator[AbstractFile] = {
    def existsFast(path: Path) = path match {
      case (_: Directory | _: File) => true
      case _ => path.exists
    }
    givenPath.toDirectory.list.filter(existsFast).map(new PlainFile(_))
  }

  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = givenPath / name
    if directory then
      if child.isDirectory /* IO! */ then
        new PlainFile(child)
      else
        null
    else if child.isFile /* IO! */ then
      new PlainFile(child)
    else
      null
  }

  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainFile(givenPath / name)
}
