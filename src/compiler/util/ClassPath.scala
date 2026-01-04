/*     ____  _____ ____                         *\
*    / __ \/ ___//  _/                         *
*   / /_/ /\__ \ / /       Psi-lang 2025       *
*  / ____/___/ // /                            *
* /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.io.FileNotFoundException
import java.util.StringTokenizer

class ClassPath (onlyPresentation: Boolean)
{
  def this() = this(false)


  /* Internal Classes * * * * * * * * * * * * * * * * * * * * * * * * * * * */
  class Source (val location: AbstractFile, val compile: Boolean) {
    override def toString(): String = "" + location + " " + compile
  }



  abstract class Entry (val location: AbstractFile) {
    def source: Source
    override def toString() = location.toString()
  }

  class Output(
    val location: AbstractFile,
    val sourceFile: AbstractFile
  ) extends Entry(location) {
    def source = if (sourceFile != null) new Source(sourceFile, true) else null
  }

  class Library(
    val location: AbstractFile
  ) extends Entry(location) {
    def doc: AbstractFile = null
    def sourceFile: AbstractFile = null
    def source = if (sourceFile != null) new Source(sourceFile, false) else null
  }

  class Context(val entries: List[Entry]) {
    def find(name: String, isDir: Boolean): Context = if (isPackage) {
      def find(entries: List[Entry]): Context =
        if (entries.isEmpty) new Context(Nil)
        else
          val ret = find(entries.tail)
          val head = entries.head
          val name_ = name + (if (!isDir) ".class" else "")
          val class_ = if (head.location == null) null else head.location.lookupPath(name_, isDir)
          val source_ = if (head.source == null) null else
            val source__ = head.source.location.lookupPath(
              name + (if (isDir) "" else ".psi"), isDir
            )
            if (source__ == null && !isDir && class_ != null) head.source.location
            else source__

          // whar
          if (class_ == null && source_ == null) ret
          else
            val entry = new Entry(class_) { // lambda class goes wild
              override def source = if (source_ == null) null else new Source(source_, head.source.compile)
            }
            new Context(entry :: ret.entries)
      end find

      val ret = find(entries)
      if (!ret.entries.isEmpty) ret
      else
        System.err.println(s"BAD_FILE: ${name} in ${this}")
        null
      
    } else null

    def isPackage: Boolean = () match
      case _ if entries.isEmpty                 => false
      case _ if entries.head.location != null   => entries.head.location.isDirectory
      case _                                    => entries.head.source.location.isDirectory


    def toString: String = toString(entries)

    def toString(entry: Entry): String = (
      (if entry.location == null
        then "<none>"
        else entry.location.toString()
      ) + 
      (if entry.source == null
        then ""
        else " with_source=" + entry.source.location.toString()
      ))

    @tailrec
    def toString(entries_ : List[Entry]): String =
      if entries_.isEmpty then ""
      else toString(entries_.head) + ":::" + toString(entries_.tail)

    def isSourceFile = {
      def head = entries.head
      def class_ = head.location
      def source = if (head.source == null) null else head.source.location
      def isPredef = source.getName.equals("psi_std.psi")

      () match
        case _ if entries.isEmpty || source == null         => false
        case _ if !onlyPresentation && !head.source.compile => false
        case _ if source.isDirectory                        => false
        case _ if class_ == null                            => true
        case _ if onlyPresentation && !isPredef             => true
        case _ if source.lastModified > class_.lastModified => true
        case _ => false
    }

    def sourceFile =
      if
        entries.head.source != null &&
        !entries.head.source.location.isDirectory
      then entries.head.source.location
      else null

    def classFile = if !isSourceFile then entries.head.location else null

    def sourcePath = if !isSourceFile && entries.head.source != null then entries.head.source.location else null

    def validPackage(name: String) = !( name.equals("META-INF") || name.startsWith(".") )
  }

  class Build {
    val entries = new ArrayBuffer[Entry]

    def root = new Context(entries.toList)
    def this(classpath: String) =
      this()
      addFilesInPath(classpath)

    def this(
      classpath: String,
      source: String,
      output: String,
      boot: String,
      extdirs: String
    ) =
      this()
      addFilesInPath(boot)
      addArchivesInExtDirPath(extdirs)
      val classes = AbstractFile.getDirectory(output)
      if classes == null then
        System.err.println(s"Output location \"${output}\" not found")
        System.exit(1) // murder
      val strtoken = new StringTokenizer(source, File.pathSeparator)
      if !strtoken.hasMoreTokens() then
        entries += new Output(classes, null)
      else while strtoken.hasMoreTokens() do
        entries += new Output(
          classes,
          AbstractFile.getDirectory(strtoken.nextToken())
        )
      addFilesInPath(classpath)

    def library(classes: String, sources: String) =
      assert(classes != null)
      val location = AbstractFile.getDirectory(classes)
      val sourceF = if sources != null then AbstractFile.getDirectory(sources) else null
      entries += new Library(location) {
        override def sourceFile = sourceF
      }

    private def addFilesInPath(p: String) =
      val strtoken = new StringTokenizer(p, File.pathSeparator)
      while strtoken.hasMoreTokens() do
        val f = AbstractFile.getDirectory(strtoken.nextToken())
        if f != null then entries != new Library(f)

    private def addArchivesInExtDirPath(p: String) =
      val strtoken = new StringTokenizer(p, File.pathSeparator)
      while strtoken.hasMoreTokens() do
        val f = AbstractFile.getDirectory(strtoken.nextToken())
        val fs = if f != null then f.toList.iterator else null
        if fs != null then while fs.hasNext do
          val f_ = fs.next().asInstanceOf[AbstractFile]
          val n = f_.getName
          if n.endsWith(".jar") || n.endsWith(".zip") then
            val a = AbstractFile.getDirectory( new File(f.getFile, n) )
            if a != null then entries != new Library(a)

    override def toString =
      entries.toList.mkString(File.pathSeparator)
  }
}
