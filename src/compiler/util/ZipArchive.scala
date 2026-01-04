/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

import java.io.{File, IOException, InputStream}
import java.util.{Enumeration, HashMap, Iterator}
import java.util.zip.{ZipEntry, ZipFile}
import scala.jdk.CollectionConverters.*

sealed trait ZipArchiveCommon
{
  def fromPath(p: String): AbstractFile = fromFile(new File(p))
  def fromFile(f: File): AbstractFile =
    try new ZipArchive(f, new ZipFile(f))
    catch
      case e: IOException => null
  
  def fromArchive(arch: ZipFile): AbstractFile = new ZipArchive(new File(arch.getName()), arch)
}


object ZipArchive extends ZipArchiveCommon


final class ZipArchive
(file: File, archive: ZipFile)
extends PlainFile(file)
   with ZipArchiveCommon
{
  private var root: DirEntry = scala.compiletime.uninitialized
  assert(archive != null)

  def isDirectory() = true

  def toList: List[Entry] =
    if root == null then load()
    return root.toList
  
  def lookupName(name: String, dir: Boolean): AbstractFile =
    if root == null then load()
    return root.lookupName(name, dir)

  private def load() =
    this.root = new DirEntry("<root>", "/")
    var  dirs = new HashMap[String, DirEntry]()
    dirs.put("/", root)

    var e: Enumeration[?] = archive.entries() // how tf does java work
    while e.hasMoreElements() do
      val entry: ZipEntry = e.nextElement().asInstanceOf[ZipEntry]
      val path = entry.getName()
      assert(entry.isDirectory() == path.endsWith("/"), s"${this}-${path}")

      if entry.isDirectory() then
        val dir = getDir(dirs, path)
        assert(dir.entry == null, s"${this} - ${path}")
        dir.entry = entry

      else
        val index = path.lastIndexOf('/')
        val name  = if index < 0 then path else path.substring(   index + 1)
        val home  = if index < 0 then "/"  else path.substring(0, index + 1)
        val paren = getDir(dirs, home)

        assert(!paren.entries.containsKey(path), s"${this} - ${path}")
        paren.entries.put(name, new FileEntry(name, path, entry))

  private def getDir(dirs: HashMap[String, DirEntry], path: String): DirEntry =
    var dir = dirs.get(path).asInstanceOf[DirEntry]
    if dir == null then
      val index = path.lastIndexOf('/', path.length() - 2)
      val name  = if index < 0 then path else path.substring(   index + 1)
      val home  = if index < 0 then "/"  else path.substring(0, index + 1)
      val paren = getDir(dirs, home)

      dir = new DirEntry(
        name.substring(0, name.length() - 1),
        path
      )
      paren.entries.put(name, dir)
      dirs.put(path, dir)
    dir






  abstract class Entry(name: String, path: String) extends VirtualFile(name, path) {
    def getPath = s"${ZipArchive.this}(${super.getPath})"
  }




  private final class DirEntry(name: String, path: String) extends Entry(name, path) {
    val entries = HashMap[String, Entry]()
    var entry: ZipEntry = null

    def isDirectory = true
    def lastMofified = if (entry != null) entry.getTime() else super.lastModified
    def toList: List[Entry] = entries.values().iterator().asScala.toList
    
    def lookupName(name: String, dir: Boolean): AbstractFile =
      entries.get(if (dir) name + "/" else name).asInstanceOf[AbstractFile]
  }

  final class FileEntry(name: String, path: String, entry: ZipEntry) extends Entry(name, path) {
    def lastModified: Long = entry.getTime()

    def read: Array[Byte] =
      var in = archive.getInputStream(entry)
      var rest = entry.getSize().toInt
      var buf = new Array[Byte](rest)
      while ({
        var res = in.read(buf, buf.length - rest, rest)
        if res == -1 then throw new IOException("Read Error")
        rest -= res
        rest > 0
      }) ()
      in.close()
      buf
  }
}
