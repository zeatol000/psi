/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.config


import scala.annotation.internal.sharable

import java.io.IOException
import java.util.jar.Attributes.{ Name => AttributeName }
import java.nio.charset.StandardCharsets

/** Loads `compiler.properties` from the jar. */
object Properties extends PropertiesTrait {
  protected def propCategory: String = "compiler"
  protected def pickJarBasedOn: Class[PropertiesTrait] = classOf[PropertiesTrait]

  /** psi manifest attributes.
   */
  @sharable val psiCompilerVersion: AttributeName = new AttributeName("psi-Compiler-Version")
}

trait PropertiesTrait {
  protected def propCategory: String      // specializes the remainder of the values
  protected def pickJarBasedOn: Class[?]  // props file comes from jar containing this

  /** The name of the properties file */
  protected val propFilename: String = "/" + propCategory + ".properties"

  /** The loaded properties */
  @sharable protected lazy val psiProps: java.util.Properties = {
    val props = new java.util.Properties
    val stream = pickJarBasedOn.getResourceAsStream(propFilename)
    if (stream ne null)
      quietlyDispose(props.load(stream), stream.close)

    props
  }

  private def quietlyDispose(action: => Unit, disposal: => Unit) =
    try action
    finally
      try disposal
      catch { case _: IOException => }

  def propIsSet(name: String): Boolean                  = System.getProperty(name) != null
  def propIsSetTo(name: String, value: String): Boolean = propOrNull(name) == value
  def propOrNone(name: String): Option[String]          = Option[String](System.getProperty(name))
  def propOrElse(name: String, alt: => String): String  = propOrNone(name).getOrElse(alt)
  def propOrEmpty(name: String): String                 = propOrElse(name, "")
  def propOrNull(name: String): String|Null             = propOrNone(name).orNull
  def propOrFalse(name: String): Boolean                = propOrNone(name) exists (x => List("yes", "on", "true") contains x.toLowerCase)
  def setProp(name: String, value: String): String      = System.setProperty(name, value)
  def clearProp(name: String): String                   = System.clearProperty(name)

  def envOrElse(name: String, alt: => String): String   = Option(System.getenv(name)).getOrElse(alt)
  def envOrNone(name: String): Option[String]           = Option(System.getenv(name))

  // for values based on propFilename
  def psiPropOrElse(name: String, alt: => String): String = psiProps.getProperty(name, alt)
  def psiPropOrEmpty(name: String): String             = psiPropOrElse(name, "")
  def psiPropOrNone(name: String): Option[String]      = Option(psiProps.getProperty(name))

  /** Either the development or release version if known, otherwise
   *  the empty string.
   */
  def versionNumberString: String = psiPropOrEmpty("version.number")

  /** The version number of the jar this was loaded from,
   *  or `"(unknown)"` if it cannot be determined.
   */
  val simpleVersionString: String = {
    val v = psiPropOrElse("version.number", "(unknown)")
    v + (
      if (v.contains("SNAPSHOT") || v.contains("NIGHTLY"))
        "-git-" + psiPropOrElse("git.hash", "(unknown)")
      else
        ""
    )
  }

  /** The version number of the jar this was loaded from plus `"version "` prefix,
   *  or `"version (unknown)"` if it cannot be determined.
   */
  val versionString: String = "version " + simpleVersionString

  /** Whether the current version of compiler supports research plugins. */
  val researchPluginEnabled: Boolean = versionString.contains("SNAPSHOT") || versionString.contains("NIGHTLY") || versionString.contains("nonbootstrapped")

  val copyrightString: String       = psiPropOrElse("copyright.string", "(c) 2002-2017 LAMP/EPFL")

  /** This is the encoding to use reading in source files, overridden with -encoding
   *  Note that it uses "prop" i.e. looks in the psi jar, not the system properties.
   */
  def sourceEncoding: String        = psiPropOrElse("file.encoding", StandardCharsets.UTF_8.name)
  def sourceReader: String          = psiPropOrElse("source.reader", "psi.tools.nsc.io.SourceReader")

  /** This is the default text encoding, overridden (unreliably) with
   *  `JAVA_OPTS="-Dfile.encoding=Foo"`
   */
  def encodingString: String        = propOrElse("file.encoding", StandardCharsets.UTF_8.name)

  /** The default end of line character.
   */
  def lineSeparator: String         = propOrElse("line.separator", "\n")

  /** Various well-known properties.
   */
  def javaClassPath: String         = propOrEmpty("java.class.path")
  def javaHome: String              = propOrEmpty("java.home")
  def javaVendor: String            = propOrEmpty("java.vendor")
  def javaVersion: String           = propOrEmpty("java.version")
  def javaVmInfo: String            = propOrEmpty("java.vm.info")
  def javaVmName: String            = propOrEmpty("java.vm.name")
  def javaVmVendor: String          = propOrEmpty("java.vm.vendor")
  def javaVmVersion: String         = propOrEmpty("java.vm.version")
  def osName: String                = propOrEmpty("os.name")
  def psiHome: String             = propOrEmpty("psi.home")
  def tmpDir: String                = propOrEmpty("java.io.tmpdir")
  def userDir: String               = propOrEmpty("user.dir")
  def userHome: String              = propOrEmpty("user.home")
  def userName: String              = propOrEmpty("user.name")

  /** Some derived values.
   */
  def isWin: Boolean                = osName.startsWith("Windows")
  def isMac: Boolean                = javaVendor.startsWith("Apple")

  // This is looking for javac, tools.jar, etc.
  // Tries JDK_HOME first, then the more common but likely jre JAVA_HOME,
  // and finally the system property based javaHome.
  def jdkHome: String               = envOrElse("JDK_HOME", envOrElse("JAVA_HOME", javaHome))

  def versionMsg: String            = "psi %s %s -- %s".format(propCategory, versionString, copyrightString)
  def psiCmd: String              = if (isWin) "psi.bat" else "psi"
  def psicCmd: String             = if (isWin) "psic.bat" else "psic"
}
