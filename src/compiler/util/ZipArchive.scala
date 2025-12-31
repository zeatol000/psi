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

final class ZipArchive
(file: File, archive: ZipFile)
extends PlainFile(file)
{
  private var root: ZipFile = scala.compiletime.uninitialized
  assert(archive != null)

  def fromPath(path: String): AbstractFile = fromFile(new File(path))
  def fromFile(f: File): AbstractFile =
    try
      return new ZipArchive(f, new ZipFile(f))
    catch
      case e: IOException => return null

  def fromArchive(arch: ZipFile): AbstractFile = new ZipArchive(new File(arch.getName()), arch)

  def isDirectory() = true

  def list() =
    if root == null then load()
    return root.list()
  
  def lookupName(name: String, dir: Boolean): AbstractFile =
    if root == null then load()
    return root.lookupName(name, dir)

  private def load() =
    this.root = new DirEntry("<root>", "/")
    var  dirs = new HashMap[String, DirEntry]()
    dirs.put("/", root)

    var e: Enumeration = archive.entries() // how tf does java work
    while e.hasMoreElements() do
      val entry: ZipEntry = e.nextElement().asInstanceOf[ZipEntry]
      val path = entry.getName()
      assert(entry.isDirector() == path.endsWith("/"), s"${this}-${path}")

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
}
