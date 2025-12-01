/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package utils

inline def void: Unit = () // useful for terminating match cases

def sysChar: Int = // system size of a char (((((NOT A JVM CHAR)))))
  if (System.getProperty("os.name").toLowerCase == "windows") 16
  else 8

