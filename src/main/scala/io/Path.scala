/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import java.io.RandomAccessFile
import java.nio.file.*
import java.net.{URI, URL}
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.io.IOException
import scala.jdk.CollectionConverters.*
import scala.util.Random.alphanumeric

object Path {
  def isExtensionJarOrZip(jpath: JPath): Boolean =
    isExtensionJarOrZip(jpath.getFileName.toString)

  def isExtensionJarOrZip(name: String): Boolean =
    fileExtension(name).isJarOrZip

  def fileExtension(name: String): FileExtension =
    val i = name.lastIndexOf(".")
    if i < 0 then FileExtension.Empty
    else FileExtension.from(name.substring(i + 1))

  def fileName(name: String): String =
    val i = name.lastIndexOf(".")
    if i < 0 then name
    else name.substring(0, i)

  def onlyDirs(xs: Iterator[Path]): Iterator[Directory] = xs.filter(_.isDirectory).map(_.toDirectory)
  def onlyFiles(xs: Iterator[Path]): Iterator[File] = xs.filter(_.isFile).map(_.toFile)

  def roots: List[Path] = FileSystems.getDefault.getRootDirectories
    .iterator().asScala.map(Path.apply).toList

  def apply(path: String): Path = apply(new java.io.File(path).toPath)
  def apply(jpath: JPath): Path = try
    if Files.isRegularFile(jpath) then new File(jpath)
    else if Files.isDirectory(jpath) then new Directory(jpath)
    else new Path(jpath)
  catch case e: SecurityException => new Path(jpath)

  private[io] def randomPrefix: String = alphanumeric take 6 mkString ""
  private[io] def fail(msg: String): Nothing = throw FileOperationException(msg)
}



class Path private[io] (val jpath: JPath) {
  import Path.*

  val separator: Char = java.io.File.separatorChar
  val separatorStr: String = java.io.File.separator

  // conversions
  def toFile: File = new File(jpath)
  def toDirectory: Directory = new Directory(jpath)
  def toAbsolute: Path = if (isAbsolute) this else new Path(jpath.toAbsolutePath)
  def toCanonical: Path = normalize.toAbsolute
  def toURI: URI = jpath.toUri()
  def toURL: URL = toURI.toURL()

  def toAbsoluteWithRoot(root: Path): Path = if (isAbsolute) this else root.toAbsolute / this

  def /(child: String): Path = new Path(jpath.resolve(child))
  def /(child: Path): Path = resolve(child)
  def /(child: Directory): Directory = /(child: Path).toDirectory
  def /(child: File): File = /(child: Path).toFile

  def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if isFile then toFile.walkFilter(cond)
    else if isDirectory then toDirectory.walkFilter(cond)
    else Iterator.empty

  def walk: Iterator[Path] = walkFilter(_ => true)

  def name: String = jpath.getFileName() match {
    case null => ""
    case name => name.toString
  }
  def path: String = jpath.toString
  def normalize: Path = new Path(jpath.normalize)

  def resolve(other: Path): Path = new Path(jpath.resolve(other.jpath))
  def relativize(other: Path): Path = new Path(jpath.relativize(other.jpath))

  def segments: List[String] = (path split separator).toList filterNot (_.length == 0)

  def parent: Directory = {
    if path.isEmpty then
      Directory("..")
    else if jpath.endsWith("..") then
      (this / "..").toDirectory
    else if jpath.endsWith(".") then
      jpath.getParent match   // strip the trailing `.` element
        case null => Directory("..")
        case p    => new Path(p).parent
    else jpath.getParent match
      case null =>
        if isAbsolute then toDirectory  // it should be a root
        else Directory(".")
      case x =>
        Directory(x)
  }
  def parents: List[Directory] = {
    val p = parent
    if p.isSame(this) then Nil else p :: p.parents
  }

  def ext: FileExtension = Path.fileExtension(name)

  def stripExtension: String = Path.fileName(name)

  def addExtension(ext: String): Path = new Path(jpath.resolveSibling(name + ext))

  def changeExtension(ext: FileExtension): Path =
    changeExtension(ext.toLowerCase)

  def changeExtension(ext: String): Path =
    val name0 = name
    val dropExtension = Path.fileName(name0)
    if dropExtension eq name0 then addExtension(ext)
    else new Path(jpath.resolveSibling(dropExtension + "." + ext))

  def ifFile[T](f: File => T): Option[T] = if (isFile) Some(f(toFile)) else None
  def ifDirectory[T](f: Directory => T): Option[T] = if (isDirectory) Some(f(toDirectory)) else None

  def canRead: Boolean = Files.isReadable(jpath)
  def canWrite: Boolean = Files.isWritable(jpath)
  def exists: Boolean = try Files.exists(jpath)  catch { case ex: SecurityException => false }
  def isFile: Boolean = try Files.isRegularFile(jpath)  catch { case ex: SecurityException => false }
  def isDirectory: Boolean =
    try Files.isDirectory(jpath)
    catch { case ex: SecurityException => jpath.toString == "." }
  def isAbsolute: Boolean = jpath.isAbsolute()
  def isEmpty: Boolean = path.length == 0

  def lastModified: FileTime = Files.getLastModifiedTime(jpath)
  def length: Long = Files.size(jpath)

  def endsWith(other: Path): Boolean = segments endsWith other.segments
  def isSame(other: Path): Boolean = toCanonical == other.toCanonical
  def isFresher(other: Path): Boolean = lastModified.compareTo(other.lastModified) > 0

  def createDirectory(force: Boolean = true, failIfExists: Boolean = false): Directory = {
    val res = tryCreate(if (force) Files.createDirectories(jpath) else Files.createDirectory(jpath))
    if (!res && failIfExists && exists) fail("Directory '%s' already exists." format name)
    else if (isDirectory) toDirectory
    else new Directory(jpath)
  }
  def createFile(failIfExists: Boolean = false): File = {
    val res = tryCreate(Files.createFile(jpath))
    Files.createFile(jpath)
    if (!res && failIfExists && exists) fail("File '%s' already exists." format name)
    else if (isFile) toFile
    else new File(jpath)
  }

  private def tryCreate(create: => JPath): Boolean =
    try { create; true } catch { case _: FileAlreadyExistsException => false }

  def delete(): Unit =
    try { Files.deleteIfExists(jpath) } catch { case _: DirectoryNotEmptyException => }

  def deleteRecursively(): Boolean = {
    if (!exists) false
    else {
      Files.walkFileTree(jpath, new SimpleFileVisitor[JPath]() {
        override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: JPath, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
      true
    }
  }

  def truncate(): Boolean =
    isFile && {
      val raf = new RandomAccessFile(jpath.toFile, "rw")
      raf.setLength(0)
      raf.close()
      length == 0
    }

  override def toString(): String = path
  override def equals(other: Any): Boolean = other match {
    case x: Path  => path == x.path
    case _        => false
  }
  override def hashCode(): Int = path.hashCode()
}
