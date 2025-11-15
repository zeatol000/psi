
/* CONFIG */
//> using scala "3.7.4"


package psi.cc

import psi.cc.*

def main(args: Array[String]): Unit =
  
  val run = process(
    cliArgs(
      args
    )
  )
  // exit 0 normally, 1 if compiler error, 2 if user error
  System.exit(
    if run eq 2 then 2
    else if run eq 1 then 1
    else 0
  )


