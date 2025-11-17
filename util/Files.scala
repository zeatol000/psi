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
import scala.util.Using
import scala.collection.mutable.ListBuffer
import java.nio.file.{Paths, Files, Path}
import java.io.*

case class File(
  path: String
){
  
  def exists: Boolean = Files.exists       (Paths.get(path))
  def isFile: Boolean = Files.isRegularFile(Paths.get(path))
  def isDir:  Boolean = Files.isDirectory  (Paths.get(path))


  def create:  Unit = if path(path.length - 1) eq '/'
    then createD
    else createF
  def createD: Unit = Files.createDirectory(Paths.get(path))
  def createF: Unit = Files.createFile     (Paths.get(path))

  
  def writeB(bytes: Seq[Byte]): Unit = write( // What the fuck is this
    (for (b <- 0 until bytes.length) yield
      if (b % 2 eq 0) && (b - 1 < bytes.length)
      then (( bytes(b) << 8 )|( bytes(b + 1) & 0xFF )).toChar
      else (( bytes(b) << 8 )|( ( 0 : Byte ) & 0xFF )).toChar
    ).toSeq)
  def write(chars: Seq[Char]): Unit =
    Using.resource(new DataOutputStream(new FileOutputStream(path))) { dataOStream =>
      chars.foreach { c =>
        dataOStream.writeChar(c)
      }
    }


  def read: Seq[Char] =
    Using.resource(new DataInputStream(new FileInputStream(path))) { DataIStream =>
      val buffer = ListBuffer[Char]()
      var reading = true
      while (reading) try {
        val c = DataIStream.readChar()
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
