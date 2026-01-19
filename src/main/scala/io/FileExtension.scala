/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package io

import psic.tools.uncheckedNN
import psic.tools.comp.util.EnumFlags.FlagSet

enum FileExtension(val toLowerCase: String):
  case Class extends FileExtension("class")
  case Jar extends FileExtension("jar")
  case Psi extends FileExtension("psi")
  case Java extends FileExtension("java")
  case Zip extends FileExtension("zip")
  case Inc extends FileExtension("inc")
  case Empty extends FileExtension("")

  /** Fallback extension */
  case External(override val toLowerCase: String) extends FileExtension(toLowerCase)

  /** represents an empty file extension. */
  def isEmpty: Boolean = this == Empty

  override def toString: String = toLowerCase

  def isClass = this == Class
  def isPsi = this == Psi
  def isJava = this == Java
  def isJar: Boolean = this == Jar
  def isZip: Boolean = this == Zip
  def isJarOrZip: Boolean = FileExtension.JarOrZip.is(this)
  def isPsiOrJava: Boolean = FileExtension.PsiOrJava.is(this)

object FileExtension:

  private val JarOrZip: FlagSet[FileExtension] = FlagSet.empty | Zip | Jar
  private val PsiOrJava: FlagSet[FileExtension] = FlagSet.empty | Psi | Java

  // this will be optimised to a single hashcode + equality check, and then fallback to slowLookup,
  // keep in sync with slowLookup.
  private def initialLookup(s: String): FileExtension = s match
    case "class" => Class
    case "jar" => Jar
    case "psi" => Psi
    case "java" => Java
    case "zip" => Zip
    case "inc" => Inc
    case _ => slowLookup(s)

  // slower than initialLookup, keep in sync with initialLookup
  private def slowLookup(s: String): FileExtension =
    if s.equalsIgnoreCase("class") then Class
    else if s.equalsIgnoreCase("jar") then Jar
    else if s.equalsIgnoreCase("psi") then Psi
    else if s.equalsIgnoreCase("java") then Java
    else if s.equalsIgnoreCase("zip") then Zip
    else if s.equalsIgnoreCase("inc") then Inc
    else External(s)

  def from(s: String): FileExtension =
    if s.isEmpty then Empty
    else initialLookup(s)
