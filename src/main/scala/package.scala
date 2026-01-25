/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic
package object tools {
 
  case class FatalError(msg: String) extends Exception(msg)

  def unsupported(name: String): Nothing =
    throw new UnsupportedOperationException(name)

  extension [T](x: T | Null)
    transparent inline def uncheckedNN: T = x.asInstanceOf[T]

  object resultWrapper {
    opaque type WrappedResult[T] = T
    def unwrap[T](x: WrappedResult[T]): T = x
    def wrap  [T](x: T): WrappedResult[T] = x
  }
  type WrappedResult[T] = resultWrapper.WrappedResult[T]
  def WrappedResult[T](x: T) = resultWrapper.wrap(x)
  def result[T](using x: WrappedResult[T]): T = resultWrapper.unwrap(x)


  transparent inline def assertShort(inline a: Boolean, inline m: Any = null): Unit =
    if !a then
      val e = if m == null then AssertionError() else AssertionError("assertion failed: " + m)
      e.setStackTrace(Array())
      throw e

}
