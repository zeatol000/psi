/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import java.net.URLClassLoader
import java.nio.file.Paths

import psic.tools.io.AbstractFileClassLoader

object ClasspathFromClassloader
{
  def apply(cl: ClassLoader): String = {
    val cpbuff = List.newBuilder[String]
    def collectCLPaths(cl: ClassLoader): Unit =
      if (cl != null) cl match {
        case cl: URLClassLoader =>
          collectCLPaths(cl.getParent)
          cpbuff ++= cl.getURLs.iterator.map(url =>
            Paths.get(url.toURI).toAbsolutePath.toString
          )
        case _ =>
          if ( cl.getClass.getName == classOf[AbstractFileClassLoader].getName )
            collectCLPaths(cl.getParent)
          else if (cl == ClassLoader.getSystemClassLoader)
            cpbuff += System.getProperty("java.class.path")
      }
    collectCLPaths(cl)
    cpbuff.result().mkString(java.io.File.pathSeparator)
  }
}
