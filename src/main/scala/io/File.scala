/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import java.io.{File => JIFile, *}
import java.nio.file.{Files, Paths}
import java.nio.file.StandardOpenOption.*

import scala.io.Codec

object File {
  def pathSeparator: String = JIFile.pathSeparator
  def separator: String = JIFile.separator

  def apply(path: String)(using Codec): File = apply(Paths.get(path))
  def apply(path: JPath)(using Codec): File = new File(path)
}

class File(jpath: JPath)(using constructorCodec: Codec)
extends Path(jpath) with Streamable.Chars {
  override val creationCodec: Codec = constructorCodec
  override def toAbsolute: File = if isAbsolute then this else super.toAbsolute.toFile
  override def toDirectory: Directory = new Directory(jpath)
  override def toFile: File = this
  override def normalize: File = super.normalize.toFile
  override def length: Long = super[Path].length
  override def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if cond(this) then Iterator.single(this) else Iterator.empty

  def inputStream: InputStream = Files.newInputStream(jpath)

  def outputStream(append: Boolean = false): OutputStream =
    if append then Files.newOutputStream(jpath, CREATE, APPEND)
    else Files.newOutputStream(jpath, CREATE, TRUNCATE_EXISTING)

  def bufferedOutput(append: Boolean = false): BufferedOutputStream = 
    new BufferedOutputStream(outputStream(append))

  def writer(append: Boolean, codec: Codec): OutputStreamWriter =
    new OutputStreamWriter(outputStream(append), codec.charSet)

  def bufferedWriter: BufferedWriter = bufferedWriter(false)
  def bufferedWriter(append: Boolean): BufferedWriter = bufferedWriter(append, creationCodec)
  def bufferedWriter(append: Boolean, codec: Codec): BufferedWriter =
    new BufferedWriter(writer(append, codec))

  def printWriter: PrintWriter = new PrintWriter(bufferedWriter, true)

  def writeAll(strings: String*): Unit =
    val out = bufferedWriter
    try strings.foreach(out.write)
    finally out.close()

  def appendAll(strings: String*): Unit =
    val out = bufferedWriter(true)
    try strings.foreach(out.write)
    finally out.close()

  def printlnAll(strings: String*): Unit =
    val out = printWriter
    try strings.foreach(out.println)
    finally out.close()

  def safeSlurp: Option[String] =
    try Some(slurp)
    catch case _: IOException => None

  /* Terrifying whatever this is */
  def setExecutable(exec: Boolean, ownerOnly: Boolean = true): Boolean =
    type JBool = java.lang.Boolean
    val method =
      try classOf[JFile].getMethod("setExecutable", classOf[Boolean], classOf[Boolean])
      catch case _: NoSuchMethodException => return false
    
    try method.invoke(jpath.toFile, exec: JBool, ownerOnly: JBool).asInstanceOf[JBool].booleanValue
    catch case _: Exception => false

}

