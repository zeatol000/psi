/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.core

/** What is the compiler doing? */
case class Mode(val bits: Int) extends AnyVal {
  import Mode.*
  def | (that: Mode): Mode = Mode(bits | that.bits)
  def & (that: Mode): Mode = Mode(bits & that.bits)
  def &~ (that: Mode): Mode = Mode(bits & ~that.bits)
  infix def is (that: Mode): Boolean = (bits & that.bits) == that.bits

  def isExpr: Boolean = (this & PatternOrTypeBits) == None

  /** Are we in the body of quoted pattern? */
  def isQuotedPattern: Boolean = (this & QuotedPatternBits) != None

  override def toString: String =
    (0 until 31).filter(i => (bits & (1 << i)) != 0).map(modeName).mkString("Mode(", ",", ")")

  def ==(that: Mode): Boolean = this.bits == that.bits
  def !=(that: Mode): Boolean = this.bits != that.bits
}

object Mode {
  val None: Mode = Mode(0)

  private val modeName = new Array[String](32)

  def newMode(bit: Int, name: String): Mode = {
    modeName(bit) = name
    Mode(1 << bit)
  }

  // Standard modes ----------------------------------------
  val Pattern: Mode = newMode(0, "Pattern")
  val Type: Mode = newMode(1, "Type")
  val Printing: Mode = newMode(2, "Printing")
  
  /** Is the user an idiot? */
  val CheckCyclic: Mode = newMode(3, "CheckCyclic")
  
  /** We are in an IDE */
  val Interactive: Mode = newMode(4, "Interactive")

  val InQuotePatternHoasArgs:      Mode = newMode(5, "InQuotePatternHoasArgs")
  val InPatternAlternative:        Mode = newMode(6, "InPatternAlternative")
  val StrictEquality:              Mode = newMode(7, "StrictEquality")
  val GadtConstraintInference:     Mode = newMode(8, "GadtConstraintInference")
  val QuotedTypePattern:           Mode = newMode(9, "QuotedTypePattern")
  val CheckBoundsOrSelfType:       Mode = newMode(10, "CheckBoundsOrSelfType")
  val AllowLambdaWildcardApply:    Mode = newMode(11, "AllowHKApplyToWildcards")
  val InPackageClauseName:         Mode = newMode(12, "InPackageClauseName")
  val InlineableBody:              Mode = newMode(13, "InlineableBody")
  val SynthesizeExtMethodReceiver: Mode = newMode(14, "SynthesizeExtMethodReceiver")
  val QuotedExprPattern:           Mode = newMode(15, "QuotedExprPattern")
  val QuotedPatternBits:           Mode = QuotedExprPattern | QuotedTypePattern
  val InExtensionMethod:           Mode = newMode(16, "InExtensionMethod")
  val InTypeTest:                  Mode = newMode(17, "InTypeTest")
  val SafeNulls:                   Mode = newMode(18, "SafeNulls")
  val ForceInline:                 Mode = newMode(19, "ForceInline")
  val InAnnotation:                Mode = newMode(20, "InAnnotation")
  val NoInline:                    Mode = newMode(21, "NoInline")
  val PatternOrTypeBits:           Mode = Pattern | Type
  val InferringReturnType:         Mode = newMode(22, "InferringReturnType")
  val TypevarsMissContext:         Mode = newMode(23, "TypevarsMissContext")


  // Psi-specific ------------------------------------------

  /** Are we in an operation instead of a function? */
  val InOperation: Mode = newMode(24, "InOperation")

  /** The function is a program */
  val InProgram: Mode = newMode(25, "InProgram")

  /** Are we in < > ? */
  val InAnomalousScope: Mode = newMode(26, "InAnomalousScope")

  /** We are lowering operations into OpenQASM, ignoring the JVM side */
  val QasmLowering: Mode = newMode(27, "QasmLowering")

  /** Treat anomalous value as mutable. If off, the anomalous value should be immutable */
  val MutableAnomaly: Mode = newMode(28, "MutableAnomaly")

  /** Is the function an op fn? */
  val InDualSynthesis: Mode = newMode(29, "InDualSynthesis")

  /** Are we creating an anomaly? */
  val AnomalyInitialization: Mode = newMode(30, "AnomalyInitialization")
}
