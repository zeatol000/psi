/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

import scala.util.control.Breaks.*
import java.io.{File, IOException}
import java.util.Iterator
import java.util.jar.JarFile
import java.util.zip.ZipFile

abstract class AbstractFile
{
  def getFile(path: String): AbstractFile = getFile(new File(path))
  def getFile(file: File  ): AbstractFile = if file.isFile() && file.exists()
    then new PlainFile(file)
    else null

  def getDirectory(path: String): AbstractFile = getDirectory(new File(path))
  def getDirectory(file: File  ): AbstractFile =
    if file.isDirectory && file.exists() then return new PlainFile(file)
    if file.isFile() && file.exists() then
      val path: String = file.getPath()
      if path.endsWith(".jar") || path.endsWith(".zip") then return ZipArchive.fromFile(file)
    null
  
  def lookupPath(path: String, dir: Boolean): AbstractFile =
    var ret: AbstractFile = null
    val len: Int = path.length()
    val sep: Char = File.separatorChar
    assert( 0 < len && path.lastIndexOf(sep) < len - 1, path )
    var file: AbstractFile = this
    var index, start = 0
    breakable:
      while (true) do
        index = path.indexOf(sep, start)
        assert( index < 0 || start < index, path+" - "+start+" - "+index )
        var name: String = path.substring(start, if (index < 0) len else index)
        file = file.lookupName(name, if (index < 0) dir else true)
        if (file == null || index < 0) then 
          ret = file
          break
    ret

  def toString: String = getPath

  def getName: String
  def getPath: String
  def getFile: File
  def isDirectory: Boolean
  def lastModified: Long
  def read: Array[Char]
  def lookupName(name: String, dir: Boolean): AbstractFile
}
