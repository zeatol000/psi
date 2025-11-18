/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

/* TODO:
 * Find a way of making the compiler ignore files that have already been compiled
 * This would also let you run a project multiple times without recompiling or packaging
 *
 * Current:
 * $ psi run main.psi -- abc      // compiles (good)
 * $ psi run main.psi -- def      // compiles again (bad)
 *
 * Better:
 * $ psi run main.psi -- abc      // compiles
 * $ psi run main.psi -- def      // doesn't compile
 * // user edits main.psi
 * $ psi run main.psi -- ghj      // compiles
 */

import psi.cc.*
import psi.cc.utils.*
import scala.io.Source
import scala.annotation.tailrec
import scala.util.{Try, Using}
import scala.util.matching.Regex
import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable.ArrayBuffer

private[cc]
class Initial(
  using Context
) extends
    Phase
{
  def phaseName: String = "Initial"
  def phaseDesc: String = "Gets the environment ready for compilation"
  def run(using Context): Unit =
  /*  This phase looks at the first psi file given and attaches in-file args to the Context */
    val firstFile = ctxt.args.psiFiles(0) + ".psi"

    ///// Build file argument parsing
    if ((!File(firstFile).exists) ||
       ( !File(firstFile).isFile)) {
      NoStackError(this, s"File $firstFile does not exist")
    }

    val xl = extractMeta(firstFile)
    var
      psi, jar, cla, jav,
      sca, kot, gro, clo
      : ArrayBuffer[String] = ArrayBuffer.empty
    var normal, compile, pack, run: ArrayBuffer[String] = ArrayBuffer.empty
    var mode: Char = 'n'

    breakable {
      for (i <- 0 until xl.length) xl(i) match {
        case s"in ${a}" => a.trim match {
          case "compile" => mode = 'c'
          case "package" => mode = 'p'
          case "run"     => mode = 'r'
          case _ => mode = 'n'          }
        case s"${a}.psi"    => psi = psi += a
        case s"${a}.jar"    => jar = jar += a
        case s"${a}.class"  => cla = cla += a
        case s"${a}.java"   => jav = jav += a
        case s"${a}.scala"  => sca = sca += a
        case s"${a}.kt"     => kot = kot += a
        case s"${a}.groovy" => gro = gro += a
        case s"${a}.clj"    => clo = clo += a
        case "--lethal-warns"
           | "--no-warns"
           | "--verbose"
           => mode match                            {
             case 'n' => normal = normal += xl(i)
             case 'c' => compile = compile += xl(i)
             case 'p' => pack = pack += xl(i)       }
          case _ => println(s"Ignoring bad argument ${xl(i)} in file ${firstFile}")
      }
    } 

    ///// Save in-file referenced files for later
    val tPsi: ArrayBuffer[String] = psi
    val tJar: ArrayBuffer[String] = jar
    val tCla: ArrayBuffer[String] = cla
    val tJav: ArrayBuffer[String] = jav
    val tSca: ArrayBuffer[String] = sca
    val tKot: ArrayBuffer[String] = kot
    val tGro: ArrayBuffer[String] = gro
    val tClo: ArrayBuffer[String] = clo

    psi = ArrayBuffer.empty
    jar = ArrayBuffer.empty
    cla = ArrayBuffer.empty
    jav = ArrayBuffer.empty
    sca = ArrayBuffer.empty
    kot = ArrayBuffer.empty
    gro = ArrayBuffer.empty
    clo = ArrayBuffer.empty

    ctxt.args.mode match {
      case 'c' => pushArgs(normal ++ compile)
      case 'p' => pushArgs(normal ++ pack)
      case 'r' => pushArgs(normal ++ run)
    }

    var invalidFiles: String = "The following files do not exist\n"
    ///// remove files that don't exist and append file type to all items
    ctxt.args.psiFiles ++ tPsi foreach { f => 
      if ! File(f + ".psi").exists then
        invalidFiles += s"\t${f + ".psi"}\n"
      else psi = psi += (f + ".psi")        }

    ctxt.args.jarFiles ++ tJar foreach { f => 
      if ! File(f + ".jar").exists then
        invalidFiles += s"\t${f + ".jar"}\n"
      else jar = jar += (f + ".jar")        }

    ctxt.args.classFiles ++ tCla foreach { f => 
      if ! File(f + ".class").exists then
        invalidFiles += s"\t${f + ".class"}\n"
      else cla = cla += (f + ".class")      }

    ctxt.args.scalaFiles ++ tSca foreach { f => 
      if ! File(f + ".scala").exists then
        invalidFiles += s"\t${f + ".scala"}\n"
      else sca = sca += (f + ".scala")      }

    ctxt.args.javaFiles ++ tJav foreach { f => 
      if ! File(f + ".java").exists then
        invalidFiles += s"\t${f + ".java"}\n"
      else jav = jav += (f + ".java")       }

    ctxt.args.kotlinFiles ++ tKot foreach { f => 
      if ! File(f + ".kt").exists then
        invalidFiles += s"\t${f + ".kt"}\n"
      else kot = kot += (f + ".kt")         }

    ctxt.args.groovyFiles ++ tGro foreach { f => 
      if ! File(f + ".groovy").exists then
        invalidFiles += s"\t${f + ".groovy"}\n"
      else gro = gro += (f + ".groovy")     }

    ctxt.args.clojureFiles ++ tClo foreach { f => 
      if ! File(f + ".clj").exists then
        invalidFiles += s"\t${f + ".clj"}\n"
      else clo = clo += (f + ".clj")        }

    if invalidFiles ne "The following files do not exist\n" then
      NoStackError(this, invalidFiles)

    ctxt.args.psiFiles      = psi.distinct
    ctxt.args.jarFiles      = jar.distinct
    ctxt.args.classFiles    = cla.distinct
    ctxt.args.javaFiles     = jav.distinct
    ctxt.args.scalaFiles    = sca.distinct
    ctxt.args.kotlinFiles   = kot.distinct
    ctxt.args.groovyFiles   = gro.distinct
    ctxt.args.clojureFiles  = clo.distinct

    vPrint("Initial phase complete")
}



///////////////////////////////////////////////////////////////////////////////
def pushArgs(args: ArrayBuffer[String])(using Context): Unit = 
  for (i <- 0 until args.length) args(i) match {
    case "--lethal-warns" =>
      ctxt.args.lethal_warns = true
    case "--no-warns" =>
      ctxt.args.no_warns = true
      ctxt.reporter.noWarns = true
    case "--verbose" =>
      ctxt.args.verbose = true
      ctxt.reporter.verbose = true
  }

///////////////////////////////////////////////////////////////////////////////
val inlineCommentRegex = """(//.*)|(/\*.*?\*/)""".r

inline def isComment(line: String): Boolean =
  line.startsWith("//") || line.startsWith("/*") || line.contains("*/")

inline def removeInlineComment(line: String): String =
  inlineCommentRegex.findFirstMatchIn(line) match {
    case Some(m) => line.substring(0, m.start).trim
    case None => line
  }

def extractMeta(path: String): List[String] =
  // FIXME: [unimportant] args in multiline comments are still considered
  Using(Source.fromFile(path)) { source =>
    val lines = source.getLines().map(_.trim).toList

    @tailrec def processLines(
      remaining: List[String], 
      buffer: List[String]
    ): List[String] = remaining match {
      case Nil => buffer
      case head :: tail =>
        val cleaned = removeInlineComment(head)
        if cleaned.startsWith(">") then
          val data = cleaned.substring(1).trim
          processLines(tail, buffer :+ data)
        else if cleaned.isEmpty || isComment(cleaned) then
          processLines(tail, buffer)
        else
          buffer
    }

    processLines(lines, List.empty[String])
  }.getOrElse(List.empty[String])


