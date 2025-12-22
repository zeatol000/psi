/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast.symbols

object Flags
{
  // modifiers
  inline val FINAL         = 0x00000001L
  inline val PUBLIC        = 0x00000002L
  inline val INHERITED     = 0x00000004L
  inline val SEALED        = 0x00000008L

  inline val OVERRIDE      = 0x00000010L
  inline val CASE          = 0x00000020L
  inline val ABSTRACT      = 0x00000040L // abstract class
  inline val DEFERRED      = 0x00000080L // abstract for members

  inline val METHOD        = 0x00000100L // function (?)
  inline val MODULE        = 0x00000200L // symbol is a module or implementing a module
  inline val INTERFACE     = 0x00000400L // java interface
  inline val MUTABLE       = 0x00000800L // mutable var
  
  inline val PARAM         = 0x00001000L // any parameter
  inline val PACKAGE       = 0x00002000L // package
  inline val DEPRECATED    = 0x00004000L // old as hell
  inline val QUANTUM       = 0x00008000L // often operation specific

  inline val COVARIANT     = 0x00010000L // corvariant type variable
  inline val CAPTURED      = 0x00010000L // accessed from nested function
  inline val BYNAMEPARAM   = 0x00010000L // param by name
  inline val CONTRAVARIANT = 0x00020000L // contravariant type variable
  inline val LABEL         = 0x00020000L // method symbol is a label
  inline val INCONSTRUCTOR = 0x00020000L // class symbol is defined in this/superclass constructor

  inline val ABSOVERRIDE   = 0x00040000L // abstract and override
  inline val LOCAL         = 0x00080000L // local to the current class // private basically

  inline val JAVA          = 0x00100000L // defined by a java class
  inline val SYNTHETIC     = 0x00200000L // compiler generated. Eg. Int + Int
  inline val STABLE        = 0x00400000L // function is probably stable
  inline val STATIC        = 0x00800000L // static modifier

  inline val TRAIT         = 0x01000000L // trait keyword
  inline val BRIDGE        = 0x02000000L // function is a bridge method
  inline val ACCESSOR      = 0x04000000L // value accessor
  inline val REFPARAM      = 0x08000000L // sharp parameters <>

  inline val SUPERACCESSOR = 0x10000000L // super.foo
  inline val PARAMACCESSOR = 0x20000000L 

  // Complicated
  inline val IS_ERROR      = 0x0000000100000000L // symbol is an error
  inline val OVERLOADED    = 0x0000000200000000L // function overload
  inline val LIFTED        = 0x0000000400000000L // class has been lifted out to package level
  inline val MIXEDIN       = 0x0000000800000000L // member has ben mixed in

  inline val EXPANDEDNAME  = 0x0000001000000000L // name with class suffix 
  inline val IMPLCLASS     = 0x0000002000000000L // symbol is an implementation class
  inline val TRANS_FLAG    = 0x0000004000000000L // transient flag guaranteed to be reset after each phase
  inline val LOCKED        = 0x0000008000000000L // temp flag to catch cyclic dependencies

  inline val InitialFlags  = 0x000000FFFFFFFFFFL // flags enabled from phase 1
  inline val LateFlags     = 0x000FFF0000000000L // flags that override flags in 0xFFF
  inline val AntiFlags     = 0x7FF0000000000000L // flags that cancel flags in 0x7FF
  inline val LateShift     = 40L
  inline val AntiShift     = 52L

  // late flags (transform phases)
  inline def latePUBLIC    = PUBLIC    << LateShift
  inline def lateDEFERRED  = DEFERRED  << LateShift
  inline def lateINTERFACE = INTERFACE << LateShift
  inline def lateMODULE    = MODULE    << LateShift
  inline def lateFINAL     = FINAL     << LateShift
  inline def lateMETHOD    = METHOD    << LateShift
  inline def notPUBLIC     = PUBLIC    << AntiShift
  inline def notINHERITED  = INHERITED << AntiShift
  inline def notABSTRACT   = ABSTRACT  << AntiShift
  inline def notOVERRIDE   = OVERRIDE  << AntiShift
  inline def notMETHOD     = METHOD    << AntiShift

  inline def STATICMODULE  = lateMODULE
  inline def STATICMEMBER  = notOVERRIDE


  // bundles
  inline def TopLevelCreationFlags = MODULE | PACKAGE | FINAL | JAVA

  inline def ExplicitFlags = PUBLIC | INHERITED | ABSTRACT | FINAL |
    SEALED | OVERRIDE | CASE | ABSOVERRIDE

  inline def PrintableFlags = ExplicitFlags | LOCAL | SYNTHETIC | STABLE |
     PARAMACCESSOR | BRIDGE | STATIC

  inline def FieldFlags = MUTABLE | PARAMACCESSOR | STATIC | FINAL

  inline def AccessFlags = PUBLIC | INHERITED
  inline def VARIANCES   = COVARIANT | CONTRAVARIANT
  inline def ConstrFlags = JAVA
  inline def PickledFlags = 0xFFFFFFFF

  inline def ModuleToClassFlags = AccessFlags | PACKAGE | CASE

  def flagsToString(flags: Long): String =
    (for i <- 0 until 64 yield flagToString(flags & (1L << i)))
      .filter(_ != "").mkString(" ")

  private def flagToString(flag: Long): String = flag match
    case IS_ERROR       => "<is-error>"
    case LIFTED         => "<lifted>"
    case MIXEDIN        => "<expandedname>"
    case IMPLCLASS      => "<implclass>"
    case TRANS_FLAG     => "<trans-flag>"
    case LOCKED         => "<locked>"
    case FINAL          => "final"
    case PUBLIC         => "pub"
    case INHERITED      => "inherited"
    case SEALED         => "sealed"
    case OVERRIDE       => "override"
    case CASE           => "case"
    case ABSTRACT       => "abstract"
    case DEFERRED       => "<deffered>"
    case METHOD         => "<method>"
    case MODULE         => "<module>"
    case INTERFACE      => "<interface>"
    case MUTABLE        => "<mutable>"
    case PARAM          => "<param>"
    case PACKAGE        => "<package>"
    case DEPRECATED     => "<deprecated>"
    case COVARIANT      => "<covariant/captured/byname>"
    case CONTRAVARIANT  => "<contravariant/label/inconstr>"
    case ABSOVERRIDE    => "abstract override"
    case LOCAL          => "<local>"
    case JAVA           => "<java>"
    case SYNTHETIC      => "<synthetic>"
    case STABLE         => "<stable>"
    case STATIC         => "<static>"
    case TRAIT          => "<trait>"
    case BRIDGE         => "<bridge>"
    case ACCESSOR       => "<accessor>"
    case SUPERACCESSOR  => "<superaccessor>"
    case PARAMACCESSOR  => "<paramaccessor>"
    case _              => ""

  class Flag(mods: Int) {
    def isPublic    = (mods & PUBLIC)    != 0
    def isInherited = (mods & INHERITED) != 0
    def isVariable  = (mods & MUTABLE)   != 0
    def isPrivate   = !isPublic && !isInherited
  }
}
