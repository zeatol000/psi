
/* CONFIG */
//> using scala "3.7.4"


package psi.cc

import psi.cc.*

val PsiVersion = "0.1.0"

def main(args: Array[String]): Unit =
  
  val run = process(
    cliArgs(
      args
    )
  )
  // exit 0 normally, 1 if compiler error, 2 if user error
  System.exit(
    if run eq 3 then
      println("Mode not implemented yet..")
      3
    else
      run.toInt
  )


