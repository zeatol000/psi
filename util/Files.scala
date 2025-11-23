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


  def create:  Unit = if path(path.length - 1) eq '/'
    then createD
    else createF
  def createD: Unit = Files.createDirectory(Paths.get(path))
  def createF: Unit = Files.createFile     (Paths.get(path))

  
  def write(chars: Seq[Byte]): Unit =
    Using.resource(new DataOutputStream(new FileOutputStream(path))) { dataOStream =>
      chars.foreach { c =>
        dataOStream.writeByte(c)
      }
    }


  def read: Seq[Byte] =
    Using.resource(new DataInputStream(new FileInputStream(path))) { DataIStream =>
      val buffer = ListBuffer[Byte]()
      var reading = true
      while (reading) try {
        val c = DataIStream.readByte()
        buffer.addOne(c.toByte)
      } catch {
        case eof: EOFException => reading = false
        case e: Exception =>
          FatalError("Utility -- File IO / read", e)
      }
      buffer.toSeq
    }
}

inline def pwd: String = System.getProperty("user.dir")
