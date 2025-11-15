/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc

import scala.collection.mutable.ArrayBuffer
import psi.cc.*


given Context = new ContextInit(
  rep,
  args
  )

def process(args: cliArgs): Byte =

  var rep = new Reporter
  rep.noWarns = cliArgs.no_warns
  rep.verbose = cliArgs.verbose

  CompilerPhases() foreach ( _.run )

  0
  // TODO: Get the reporter and change the exit code depending on 
  // errors, warns, and compiler exceptions



private[cc]
class cliArgs
{ //! args must be vars since in-file declaration exists
  /* mode/s */
  var
    comp,
    pack,
    repl,
    clean,
    tree
    : Boolean = false
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
  var // other
    lethal_warns,
    no_warns,
    verbose
    : Boolean = false
}

object cliArgs extends cliArgs
{
  def apply(xa: Array[String]): cliArgs =
    var ret = new cliArgs
    var
      psi, jar, cla, jav,
      sca, kot, gro, clo
      : ArrayBuffer[String] = ArrayBuffer.empty
  
    // pushing args
    xa foreach { x => x match {
      case "compile"  => ret.comp   = true
      case "package"  => ret.pack   = true
      case "repl"     => ret.repl   = true
      case "clean"    => ret.clean  = true
      case "tree"     => ret.tree   = true

      case s"${a}.psi"     => psi = psi += a
      case s"${a}.jar"     => jar = jar += a
      case s"${a}.class"   => cla = cla += a
      case s"${a}.java"    => jav = jav += a
      case s"${a}.scala"   => sca = sca += a
      case s"${a}.kt"      => kot = kot += a
      case s"${a}.groovy"  => gro = gro += a
      case s"${a}.clj"     => clo = clo += a

      case "--lethal-warns" => ret.lethal_warns = true
      case "--no-warns"     => ret.no_warns = true
      case "--verbose"      => ret.verbose = true
      case _ => println(s"Ignoring bad argument ${x}")
    }}

    ret.psiFiles      = psi
    ret.jarFiles      = jar
    ret.classFiles    = cla
    ret.javaFiles     = jav
    ret.scalaFiles    = sca
    ret.kotlinFiles   = kot
    ret.groovyFiles   = gro
    ret.clojureFiles  = clo

    return ret
}
