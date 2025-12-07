/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

/**
 * File management
 */

import psi.cc.*
import scala.util.{Using, Try, Failure, Success}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.nio.file.{Paths, Files, Path}
import java.io.*

case class File(
  path: String
){
  
  def exists: Boolean = Files.exists       (Paths.get(path))
  def isFile: Boolean = Files.isRegularFile(Paths.get(path))
  def isDir:  Boolean = Files.isDirectory  (Paths.get(path))

  def encoding: Int =
    val bom = Files.readAllBytes(Paths.get(path)).take(4)
    if      bom.sameElements(Array(0xFE, 0xFF))       then 16
    else if bom.sameElements(Array(0xFF, 0xFE))       then 16
    else if bom.sameElements(Array(0xEF, 0xBB, 0xBF)) then 8
    else 8

  def create:  Unit = if path(path.length - 1) eq '/'
    then createD
    else createF
  def createD: Unit = Files.createDirectory(Paths.get(path))
  def createF: Unit = Files.createFile     (Paths.get(path))

  
  def write(chars: Seq[Char]): Unit =
    Using.resource(new DataOutputStream(new FileOutputStream(path))) { dataOStream =>
      chars.foreach { c =>
        if (this.encoding == 8) dataOStream.writeByte(c.toByte)
        else dataOStream.writeChar(c)
      }
    }


  def read: Seq[Char] =
    Using.resource(new DataInputStream(new FileInputStream(path))) { DataIStream =>
      val buffer = ListBuffer[Char]()
      var reading = true
      while (reading) try {
        val c = (if this.encoding == 8 then // Support windows appearantly
          DataIStream.readByte().toChar
          else DataIStream.readChar() ) 
        buffer.addOne(c)
      } catch {
        case eof: EOFException => reading = false
        case e: Exception =>
          FatalError("Utility -- File IO / read", e)
      }
      buffer.toSeq
    }
}

inline def pwd: String = System.getProperty("user.dir")
