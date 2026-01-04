/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

// wtf was the og scala compiler doing with this file...

class VirtualFile(
  private val name: String,
  private val path: String
) extends AbstractFile
{
  assert( name != null && path != null, s"$name - $path" )


  def getName = name
  def getPath = path
  def isDirectory = false
  def lastModified: Long = Long.MinValue
  final def getFile: File = null
  
  def read: Array[Byte] =
    assert(!isDirectory, s"Cannot read directory '${this}'")
    new Array[Byte](0)

  def toList: List[AbstractFile] =
    assert(isDirectory, s"Not a directory '${this}'")
    List.empty

  def lookupName(name: String, dir: Boolean): AbstractFile =
    assert(isDirectory, s"Not a directory '${this}'")
    null
}
