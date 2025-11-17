/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

/**
 * This file handles parsing CLI args and compilation modes
 * If the mode is compilation or packaging,
 */

import psi.cc.*
import psi.cc.utils.*
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

def process(args: cliArgs): Byte =
   
  args.mode match { // TODO: [unimportant] make the repl and cleaning

    case 'r' => println("Opening REPL"); 3
    case 'l' => println("Cleaning class and other compilation files"); 3

    case 'c' | 'p' =>
      var rep = new Reporter //! basically all compiler args must be created here
      rep.noWarns = args.no_warns
      rep.verbose = args.verbose
  
      given Context =
        new ContextInit( // Context creation
          rep,
          args
        )
      
      val phases: List[Phase] = CompilerPhases
      phases foreach ( _._run )

      rep.exitCode
    
    case _ => println(s"""
      |Fatal error during compilation..
      |
      |Error details:
      |   Psi Version -> $PsiVersion
      |   During Phase -> CLI Parsing
      |   Exception Message -> Internal error: mode ${args.mode} does not exist
      """)
      1
  }





///////////////////////////////////////////////////////////////////////////////

private[cc]
class cliArgs
{ //! NOTE: args must be vars since in-file declaration exists
  
  var mode: Char = ' '
  var // inlcuded files
    psiFiles,
    jarFiles,
    classFiles,
    javaFiles,
    scalaFiles,
    kotlinFiles,
    groovyFiles,
    clojureFiles
    : ArrayBuffer[String] = ArrayBuffer.empty
  var
    lethal_warns,
    no_warns,
    verbose
    : Boolean = false
  var args: Array[String] = Array()

  // TODO: format this better.
  def dump: String = s"""
  |Mode: $mode
  |   [c]ompile
  |   [p]ackage
  |   [r]epl
  |   c[l]ean
  |Files
  |   jars: $jarFiles
  |   classes: $classFiles
  |   psi: $psiFiles
  |   scala: $scalaFiles
  |   java: $javaFiles
  |   kotlin: $kotlinFiles
  |   goovy: $groovyFiles
  |   clojure: $clojureFiles
  |Flags
  |   lethal-warns: $lethal_warns
  |   no-warns: $no_warns
  |   verbose: $verbose
  |Pushed Args
  |   ${args.mkString(" ")}
  """.stripMargin
}





///////////////////////////////////////////////////////////////////////////////

object cliArgs extends cliArgs
{
  // TODO: Make wildcard files and directories work
  // Also check if files exist first
  def apply(xa: Array[String]): cliArgs =
    var ret = new cliArgs
    var
      psi, jar, cla, jav,
      sca, kot, gro, clo
      : ArrayBuffer[String] = ArrayBuffer.empty
  
    // pushing args
    breakable {
      for (i <- 0 until xa.length) xa(i) match {
        case "compile" if (ret.mode eq ' ') => ret.mode   = 'c'
        case "package" if (ret.mode eq ' ') => ret.mode   = 'p'
        case "repl"    if (ret.mode eq ' ') => ret.mode   = 'r'
        case "clean"   if (ret.mode eq ' ') => ret.mode   = 'l'

        case s"${a}.psi"     => psi = psi += a
        case s"${a}.jar"     => jar = jar += a
        case s"${a}.class"   => cla = cla += a
        case s"${a}.java"    => jav = jav += a
        case s"${a}.scala"   => sca = sca += a
        case s"${a}.kt"      => kot = kot += a
        case s"${a}.groovy"  => gro = gro += a
        case s"${a}.clj"     => clo = clo += a

        //! NOTE: For flags, when a new one is added, remember to 
        //! add the flag to the initial phase parser and setters
        //! for the class
        case "--lethal-warns" => ret.lethal_warns = true
        case "--no-warns"     => ret.no_warns = true
        case "--verbose"      => ret.verbose = true
        case "--"             => ret.args = xa.drop(i + 1); break()
        case _ => println(s"Ignoring bad argument ${xa(i)}")
      }
    }

    ret.psiFiles      = psi.filter(_ != "")
    ret.jarFiles      = jar.filter(_ != "")
    ret.classFiles    = cla.filter(_ != "")
    ret.javaFiles     = jav.filter(_ != "")
    ret.scalaFiles    = sca.filter(_ != "")
    ret.kotlinFiles   = kot.filter(_ != "")
    ret.groovyFiles   = gro.filter(_ != "")
    ret.clojureFiles  = clo.filter(_ != "")

    if ( ret.psiFiles.length eq 0 ) && // why the hell are there so many parenthesis here
      (( ret.mode eq 'c'  ) ||
      (  ret.mode eq 'p' )) then
      NoStackError("CLI Parsing", "At least 1 psi file is needed for compilation and packaging modes")

    return ret
}
