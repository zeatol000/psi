/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import scala.language.unsafeNulls

import java.io.{IOException, InputStream, OutputStream,
  BufferedOutputStream, ByteArrayOutputStream}
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files, Paths}

object AbstractFile {

  def getFile(path: String): AbstractFile = getFile(File(path))
  def getFile(path: JPath ): AbstractFile = getFile(File(path))
  def getDirectory(path: String): AbstractFile = getDirectory(Directory(path))
  def getDirectory(path: JPath ): AbstractFile = getDirectory(Directory(path))

  def getFile(path: Path): AbstractFile =
    if (path.isFile) new PlainFile(path) else null

  def getDirectory(path: Path): AbstractFile =
    if (path.isDirectory) new PlainFile(path)
    else if (path.isFile && Path.isExtensionJarOrZip(path.jpath)) ZipArchive.fromFile(path.toFile)
    else null

  def getURL(url: URL): AbstractFile =
    if (url.getProtocol != "file") null
    else new PlainFile(new Path(Paths.get(url.toURI)))

  def getResources(url: URL): AbstractFile = ZipArchive.fromManifestURL(url)
}



abstract class AbstractFile extends Iterable[AbstractFile] {
  def name: String
  def path: String
  def absolutePath: String = path
  def canonicalPath: String = if (jpath == null) path else jpath.normalize.toString

  val ext: FileExtension = Path.fileExtension(name)
  
  def absolute: AbstractFile
  def container: AbstractFile

  def file: JFile | Null = try {
    if (jpath == null) null
    else jpath.toFile
  } catch {
    case _: UnsupportedOperationException => null
  }

  def jpath: JPath | Null

  def underlyingSource: Option[AbstractFile] = None

  def exists: Boolean = (jpath == null) || Files.exists(jpath)

  def isClassContainer: Boolean = isDirectory ||
    (jpath != null && ext.isJarOrZip)

  def isDirectory: Boolean

  def isVirtual: Boolean = false

  def lastModified: Long

  def input: InputStream
  def output: OutputStream
  def bufferedOutput: BufferedOutputStream = new BufferedOutputStream(output)

  def sizeOption: Option[Int] = None

  def toURL: URL = if (jpath == null) null else jpath.toUri.toURL

  @throws(classOf[IOException])
  def toCharArray: Array[Char] = new String(toByteArray).toCharArray

  @throws(classOf[IOException])
  def toByteArray: Array[Byte] =
    val in = input
    sizeOption match {
      case Some(size) =>
        var rest = size
        val arr = new Array[Byte](rest)
        while (rest > 0) {
          val res = in.read(arr, arr.length - rest, rest)
          if (res == -1) throw new IOException("read error")
          rest -= res
        }
        in.close()
        arr
      case None =>
        val out = new ByteArrayOutputStream()
        var c = in.read()
        while (c != -1) {
          out.write(c)
          c = in.read()
        }
        in.close()
        out.toByteArray()
    }


  def iterator: Iterator[AbstractFile]

  final def lookupPath(parts: Seq[String], directory: Boolean): AbstractFile | Null =
    var f: AbstractFile = this
    var i = 0
    val n = parts.length - 1
    while (f != null && i < n) {
      f = f.lookupName(parts(i), directory = true)
      i += 1
    }
    if f == null then null else f.lookupName(parts(i), directory = directory)



  def lookupName(name: String, directory: Boolean): AbstractFile
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile

  def lookupPathUnchecked(path: String, directory: Boolean): AbstractFile =
    lookup( (f, p, d) => f.lookupNameUnchecked(p, d), path, directory )

  private def lookup(
    getFile: (AbstractFile, String, Boolean) => AbstractFile,
    path0: String, directory: Boolean
  ): AbstractFile =
    val separator = java.io.File.separatorChar
    val path: String = if (path0.last == separator) path0.dropRight(1) else path0
    val len = path.length
    assert(len > 0 && !(path.last == separator), path)
    var file = this
    var start = 0
    while true do
      val i = path.indexOf(separator, start)
      assert(i < 0 || start < i,  ((path, directory, start, i)))
      val n = path.substring(start, if i < 0 then len else i)
      file = getFile(file, n, if i < 0 then directory else true)
      if file == null || i < 0 then return file
      start = i + 1
    file

  final def resolveSibling(name: String): AbstractFile | Null =
    container.lookupName(name, directory = false)

  final def resolveSiblingWithExtension(extension: FileExtension): AbstractFile | Null =
    resolveSibling(Path.fileName(name) + "." + extension)
  
  private def fileOrSubdirectoryNamed(name: String, dir: Boolean): AbstractFile =
    lookupName(name, dir) match
      case null =>
        try Files.createDirectories(jpath)
        catch case _: FileAlreadyExistsException if Files.isDirectory(jpath) => ()

        val p = jpath.resolve(name)
        try
          if dir then Files.createDirectory(p)
          else Files.createFile(p)
        catch case _: FileAlreadyExistsException => ()
        new PlainFile(new File(p))
      case lookup => lookup

  def fileNamed(name: String): AbstractFile =
    assert(isDirectory, s"Tried to find '${name}' in '${path}' but it is not a directory")
    fileOrSubdirectoryNamed(name, dir = false)

  def subdirectoryNamed(name: String): AbstractFile =
    assert(isDirectory, s"Tried to find '$name' in '$path' but it is not a directory")
    fileOrSubdirectoryNamed(name, dir = true)

  protected def unsupported(): Nothing = unsupported(null)
  protected def unsupported(msg: String): Nothing =
    throw new UnsupportedOperationException(msg)

  override def toString(): String = path
}
