/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.core

object Flags {
  object opaques {
    opaque type FlagSet = Long
    def FlagSet(x: Long): FlagSet = x
    def toBits(x: FlagSet): Long = x

    opaque type Flag <: FlagSet = Long
    private[Flags] def Flag(bits: Long): Flag = bits
  }
  import opaques.FlagSet
  import opaques.Flag
  export opaques.FlagSet
  export opaques.Flag



  extension (x: FlagSet) {
    inline def bits: Long = opaques.toBits(x)

    def | (y: FlagSet): FlagSet = () match
      case _ if x.bits == 0 => y
      case _ if y.bits == 0 => x
      case _ =>
        val total = x.bits & y.bits & KINDFLAGS
        assert(total != 0, s"illegal flagset combination: ${x.flagsString} and ${y.flagsString}")
        FlagSet(total | ((x.bits | y.bits) & ~KINDFLAGS))

    def & (y: FlagSet): FlagSet = FlagSet(x.bits & y.bits)

    def &~ (y: FlagSet): FlagSet =
      val total = x.bits & KINDFLAGS
      if (total & y.bits) == 0 then x
      else FlagSet(total | ((x.bits & ~y.bits) & ~KINDFLAGS))

    def ^ (y: FlagSet) = FlagSet((x.bits | y.bits) & KINDFLAGS | (x.bits ^ y.bits) & ~KINDFLAGS)

    infix def is (f: Flag): Boolean =
      val fs = x.bits & f.bits
      (fs & KINDFLAGS) != 0 && (fs & ~KINDFLAGS) != 0

    def is (f: Flag, n: FlagSet): Boolean = x.is(f) && !x.isOneOf(n)

    infix def isOneOf(f: FlagSet): Boolean =
      val fs = x.bits & f.bits
      (fs & KINDFLAGS) != 0 && (fs & ~KINDFLAGS) != 0

    def isOneOf(f: FlagSet, n: FlagSet): Boolean = x.isOneOf(f) && !x.isOneOf(n)

    def isAllOf(f: FlagSet): Boolean =
      val fs = x.bits & f.bits
      ((fs & KINDFLAGS) != 0 || f.bits == 0) &&
      (fs >>> TYPESHIFT) == (f.bits >>> TYPESHIFT)

    def isAllOf (flags: FlagSet, butNot: FlagSet): Boolean = x.isAllOf(flags) && !x.isOneOf(butNot)

    def isEmpty: Boolean = (x.bits & ~KINDFLAGS) == 0

    def <= (y: FlagSet): Boolean = (x.bits & y.bits) == x.bits

    def isTermFlags: Boolean = (x.bits & TERMS) != 0

    def isTypeFlags: Boolean = (x.bits & TYPES) != 0

    def toTypeFlags: FlagSet = if (x.bits == 0) x else FlagSet(x.bits & ~KINDFLAGS | TYPES)

    def toTermFlags: FlagSet = if (x.bits == 0) x else FlagSet(x.bits & ~KINDFLAGS | TERMS)

    def toCommonFlags: FlagSet = if (x.bits == 0) x else FlagSet(x.bits | KINDFLAGS)

    def numFlags: Int = java.lang.Long.bitCount(x.bits & ~KINDFLAGS)

    def firstBit: Int = java.lang.Long.numberOfTrailingZeros(x.bits & ~KINDFLAGS)

    private def flagString(i: Int): List[String] =
      if (x.bits & (1L << i)) == 0 then Nil
      else
        def hs(k: Int) = if (x.bits & (1L << k)) != 0 then flagName(i)(k) else ""
        val termfs = hs(TERMindex)
        val typefs = hs(TYPEindex)
        val strs = termfs :: (if (termfs == typefs) Nil else typefs :: Nil)
        strs.filter(_.nonEmpty)

    def flagStrings(pub: String = ""): Seq[String] =
      val raw   = (2 to MaxFlag).flatMap(x.flagString(_))
      val scope = if x is Local then "this" else pub

      if (scope != "")
        raw.filter(_ != "<local>").map:
          case "pub" => s"pub[$scope]"
          case "pro" => s"pro[$scope]"
          case s => s;
      else raw.filter(_ != "<local>")

    def flagsString: String = x.flagStrings("").mkString(" ")
  }

  
  inline def or(x: FlagSet, y: FlagSet) = x | y
  inline def and(x: FlagSet, y: FlagSet) = x & y
  inline def termFlagSet(x: Long) = FlagSet(TERMS | x)
  
  private inline val TYPESHIFT = 2
  private inline val TERMindex = 0
  private inline val TYPEindex = 1
  private inline val TERMS = 1 << TERMindex
  private inline val TYPES = 1 << TYPEindex
  private inline val KINDFLAGS = TERMS | TYPES
  private inline val FirstFlag = 2
  private inline val FirstNotPickledFlag = 48  // may need changing
  private inline val MaxFlag = 63

  private val flagName = Array.fill(64, 2)("")
  private def isDefinedAsFlag(i: Int) = flagName(i).exists(_.nonEmpty)

  private def flagRange(start: Int, end: Int) = FlagSet(
    (start until end).foldLeft(KINDFLAGS.toLong) {(bits, idx) =>
      if isDefinedAsFlag(idx) then bits | (1L << idx) else bits
    }
  )

  def union(fs: FlagSet*): FlagSet =
    var flag = EmptyFlags
    for f <- fs do flag |= f
    flag

  def commonFlags(fs: FlagSet*): FlagSet = union(fs.map(_.toCommonFlags)*)

  val EmptyFlags = FlagSet(0)
  val UndefinedFlags = FlagSet(~KINDFLAGS)

  private def newFlags(i: Int, name: String, typeName: String = ""): (Flag, Flag, Flag) =
    flagName(i)(TERMindex) = name
    flagName(i)(TYPEindex) = if typeName.isEmpty then name else typeName
    val bits = 1L << i
    ( opaques.Flag(KINDFLAGS | bits),
      opaques.Flag(TERMS | bits),
      opaques.Flag(TYPES | bits))


  // Flag definitions ---------------------------------------------------------------------------------------
  // (Common, Term, Type)
  val (Public@ _, PublicTerm@ _, PublicType@ _)             = newFlags(2,  "pub")
  val (Protected@ _, _, _)                                  = newFlags(3,  "pro")
  
  val (Override@ _, _, _)                                   = newFlags(4,  "override")
  val (Deferred@ _, DeferredTerm@ _, DeferredType@ _)       = newFlags(5,  "<deferred>")
  val (Final@ _, _, _)                                      = newFlags(6,  "final")
  val (_, Method@ _, _)                                     = newFlags(7,  "<method>")
  val (Param@ _, TermParam@ _, TypeParam@ _)                = newFlags(8,  "<param>")
  val (LazyOrTrait@ _, Lazy@ _, Trait@ _)                   = newFlags(9,  "<lazy>", "<trait>")

  /** Any method declared with op instead of fn. Compiles to OpenQASM */
  val (_, Operation@ _, _)                                  = newFlags(10, "op")

  /** TERM: Any method declared with op fn. Compiles to both JVM and OpenQASM.
   * Whichever method is called depends on the caller type; a function will call a function and an operation will call the operation
   *
   ** TYPE: Any class/trait that can be anomalous
   */
  val (Dual@ _, DualTerm@ _, DualType@ _)                   = newFlags(11, "<dual>")

  val (AccessorOrSealed@ _, Accessor@ _, Sealed@ _)         = newFlags(12, "<accessor>", "sealed")
  val (MutableOrOpen@ _, Mutable@ _, Open@ _)               = newFlags(13, "mutable", "open")

  val (Local@ _, _, _)                                      = newFlags(14, "<local>")

  /** Anomalous values are values that must be given an exact copy when passed
   * eg in {{{let q = Qubit; H<q>}}} the function H<> must be given q, not just the value or reference
   *
   * Anomalous Terms are defined with any of
   *  - let x: T = ...
   *  - fn x: <T> = ...
   *  - op x: <T> = ...
   */
  val (Anomalous@ _, AnomalousTerm@ _, AnomalousType@ _)    = newFlags(15, "<anomalous>")

  /** As shown in the anomalous values, copy args are used to differentiate between classical and anomalous
   *  value passing
   */
  val (_, CopyArg@ _, _)                                    = newFlags(16, "<copyarg>")
  
  val (ParamAccessorOrInto@ _, ParamAccessor@ _, Into@ _)   = newFlags(17, "<paramaccessor>", "<into>")
  val (Module@ _, ModuleVal@ _, ModuleClass@ _)             = newFlags(18, "module")
  val (Package@ _, PackageVal@ _, PackageClass@ _)          = newFlags(19, "<package>")
  val (_, _, StructClass@ _)                                = newFlags(20, "struct") // case classes

  /** Programs are declared with pro, the same keyword as protected. 
   * When programs are parsed, they are added to a list.
   * During compilation, a synthetic function is created that creates separate threads for all programs.
   * If a program is told to return a number, then that number will be turned into an integer and call System.exit.
   * Void programs will just terminate the thread at the end of execution.
   * One program will occupy the main thread, so there aren't too many threads active
   */
  val (_, Program@ _, _)                                    = newFlags(21, "<program>")

  val (Synthetic@ _, _, _)                                  = newFlags(22, "<synthetic>")
  val (Inline@ _, _, _)                                     = newFlags(23, "inline")
  val (OuterOrCovariant@ _, OuterAccessor@ _, Covariant@ _) = newFlags(24, "<outer accessor>", "<covariant>")
  val (LabelOrContravariant@ _, Label@ _, Contravariant@ _) = newFlags(25, "<label>", "<contravariant>")
  val (_, AbsOverride@ _, PureInterface@ _)                 = newFlags(26, "abstract override", "interface")
  val (Abstract@ _, _, _)                                   = newFlags(27, "abstract")

  val (_, StableRealizable@ _, _)                           = newFlags(28, "<stable>")
  val (_, HasDefault@ _, Impure@ _)                         = newFlags(29, "<hasdefault>", "<impure>")
  val (Extension@ _, ExtensionMethod@ _, _)                 = newFlags(30, "<extension>")
  val (JavaDefined@ _, JavaDefinedVal@ _, _)                = newFlags(31, "<java>")
  val (JavaStatic@ _, JavaStaticTerm@ _, JavaStaticType@ _) = newFlags(32, "<static>")
  val (_, Captured@ _, NoInits@ _)                          = newFlags(33, "<captured>", "<noinits>")
  val (Artifact@ _, _, _)                                   = newFlags(34, "<artifact>")
  val (_, Bridge@ _, _)                                     = newFlags(35, "<bridge>")
  val (_, InlineProxy@ _, _)                                = newFlags(36, "<inline proxy>")
  val (_, Synchronized@ _, _)                               = newFlags(37, "<synchronized>")
  val (_, JavaVarargs@ _, JavaAnnotation@ _)                = newFlags(38, "<varargs>", "<java-annotation>")
  val (_, DefaultMethod@ _, _)                              = newFlags(39, "<defaultmethod>")
  val (Transparent@ _, _, TransparentType@ _)               = newFlags(40, "transparent")
  val (Enum@ _, EnumVal@ _, _)                              = newFlags(41, "enum")
  val (Exported@ _, ExportedTerm@ _, ExportedType@ _)       = newFlags(42, "exported")
  val (Erased@ _, _, _)                                     = newFlags(43, "erased")
  val (Invisible@ _, _, _)                                  = newFlags(44, "<invisible>")
  val (Tracked@ _, _, Dependent@ _)                         = newFlags(45, "tracked")

  /** Marks op fn methods that have already been expanded */
  val (_, GeneratedDual@ _, _)                              = newFlags(46, "<generated-dual>")
 
  /** fn new(): <this.type> */
  val (_, AnomalousConstructor@ _, _)                       = newFlags(47, "<anomalous-init>")
  
  /** Types that are only accessible by purely quantum methods */
  val (_, _, QuantumPrimitive@ _)                           = newFlags(48, "<quantum-primitive>")

  val (NonMember@ _, _, _)                                  = newFlags(49, "<non-member>")
  val (Touched@ _, _, _)                                    = newFlags(50, "<touched>")
  val (Lifted@ _, _, _)                                     = newFlags(51, "<lifted>")
  val (MixedIn@ _, _, _)                                    = newFlags(52, "<mixedin>")
  val (Specialized@ _, _, _)                                = newFlags(53, "<specialized>")
  val (_, SelfName@ _, _)                                   = newFlags(54, "<selfname>")
  val (_, _, ChildrenQueried@ _)                            = newFlags(56, "<children-queried>")
  val (_, HasDefaultParams@ _, _)                           = newFlags(57, "<has-default-params>")
  val (_, NoDefaultParams@ _, Provisional@ _)               = newFlags(60, "<no-default-params>", "<provisional>")
  val (Permanent@ _, _, _)                                  = newFlags(61, "<permanent>")
  val (PhantomSymbol@ _, _, _)                              = newFlags(62, "<phantom symbol>")

  // combined flag sets -------------------------------------------------------------------------------------------------
  
  val AnyFlags: FlagSet = flagRange(FirstFlag, MaxFlag)

  val PickledFlags: FlagSet = flagRange(FirstFlag, FirstNotPickledFlag)

  val AccessFlags: FlagSet = Local | Public | Protected

  private val CommonModifierFlags: FlagSet =
    commonFlags(Public, Protected, Final, Override, JavaStatic, Transparent, Erased)

  val TypeModifierFlags: FlagSet =
    CommonModifierFlags.toTypeFlags | Abstract | Sealed | Open | StructClass | DualType | Trait

  val TermModifierFlags: FlagSet =
    CommonModifierFlags.toTermFlags | 
    Inline | AbsOverride | Lazy | Operation | DualTerm | AnomalousTerm | Program

  val QuantumFlags: FlagSet = Operation | DualTerm

  val IdentityFlags: FlagSet = AnomalousTerm | CopyArg

  val EntryPointFlags: FlagSet = Program | Synthetic
  val ThreadSpawnerFlags: FlagSet = Synthetic | Method | Public | JavaStatic

  val FromStartFlags: FlagSet = commonFlags(
    Module, Package, Deferred, Method, Enum, Param, ParamAccessorOrInto,
    MutableOrOpen, Touched, JavaStatic, OuterOrCovariant,
    LabelOrContravariant, Tracked, Extension, NonMember,
    Permanent, Synthetic, Exported, Inline, PhantomSymbol, Invisible)

  val AfterLoadFlags: FlagSet = commonFlags(
    FromStartFlags, AccessFlags, Final, AccessorOrSealed,
    Abstract, LazyOrTrait, SelfName, JavaDefined, JavaAnnotation, Transparent)

  val UnstableValueFlags: FlagSet = Mutable | Method

  val VarianceFlags: FlagSet = Covariant | Contravariant


  // creation flags -----------------------------------------------------------------------------------------------------

  val LetCreationFlags: FlagSet = AnomalousTerm | Mutable

  val ProgramCreationFlags: FlagSet = Program | Method | Final | Public

  val StructCreationFlags: FlagSet = StructClass | Final | Public

  val DualMethodCreationFlags: FlagSet = Method | DualTerm

  val ModuleValCreationFlags: FlagSet = ModuleVal | Lazy | Final | StableRealizable

  val ModuleClassCreationFlags: FlagSet = ModuleClass | Final

  val AccessorCreationFlags: FlagSet = Method | Accessor

  val PureInterfaceCreationFlags: FlagSet = Trait | NoInits | PureInterface

  val SelfSymFlags: FlagSet = Local | Deferred

  val ClassTypeParamCreationFlags: FlagSet =
    TypeParam | Deferred | Local

  val PackageCreationFlags: FlagSet =
    Module | Package | Final | JavaDefined

  // retained flags ------------------------------------------------------------------------------------------------------

  val RetainedTypeArgFlags: FlagSet = VarianceFlags | Public | Protected | Local

  val RetainedDualFlags: FlagSet = AccessFlags | DualTerm | DualType | Final | Inline | Transparent

  val RetainedModuleValAndClassFlags: FlagSet =
    AccessFlags | Package |
    Synthetic | JavaDefined | JavaStatic | Artifact |
    Lifted | MixedIn | Specialized | PhantomSymbol | Invisible

  val RetainedModuleValFlags: FlagSet = RetainedModuleValAndClassFlags |
    Override | Final | Method | Lazy | Erased |
    Accessor | AbsOverride | StableRealizable | Captured | Synchronized | Transparent

  val RetainedModuleClassFlags: FlagSet = RetainedModuleValAndClassFlags | Enum

  val RetainedExportTermFlags = Inline | Transparent | HasDefaultParams | NoDefaultParams | ExtensionMethod

  val RetainedExportTermParamFlags = Erased | HasDefault | Inline

  val MandatoryExportTermFlags = Exported | Method | Final

  val ClassOnlyFlags = Sealed | Open | Abstract.toTypeFlags | StructClass | DualType | AnomalousType

  // Other flags -----------------------------------------------------------------------------------------------------
  
  val AnomalousParam: FlagSet = Param | CopyArg | AnomalousTerm
  val PureOperation: FlagSet = Method | Operation
  val DualOperation: FlagSet = Method | DualTerm
  val AnomalousMutable: FlagSet = AnomalousTerm | Mutable
  val QasmLegalType: FlagSet = QuantumPrimitive | (AnomalousType & DualType)

  val ProgramFlags: FlagSet = Method | Program | Final | Public
  val IllegalProgramFlags: FlagSet = Program | Operation

  val EffectivelyAnomalous: FlagSet = AnomalousTerm | CopyArg
  
  val NotConcrete: FlagSet                   = AbsOverride | Deferred
  val AbstractFinal: FlagSet                 = Abstract | Final
  val AbstractOverride: FlagSet              = Abstract | Override
  val AbstractSealed: FlagSet                = Abstract | Sealed
  val AbstractOrTrait: FlagSet               = Abstract | Trait
  val AccessorOrDeferred: FlagSet            = Accessor | Deferred
  val PrivateAccessor: FlagSet               = Accessor
  val AccessorOrSynthetic: FlagSet           = Accessor | Synthetic
  val JavaOrPrivateOrSynthetic: FlagSet      = Artifact | JavaDefined | Synthetic
  val PrivateOrSynthetic: FlagSet            = Artifact | Synthetic
  val CovariantLocal: FlagSet                = Covariant | Local                              // A covariant type parameter
  val ContravariantLocal: FlagSet            = Contravariant | Local                          // A contravariant type parameter
  val ConstructorProxyModule: FlagSet        = PhantomSymbol | Module
  val CaptureParam: FlagSet                  = PhantomSymbol | StableRealizable | Synthetic
  val DefaultParameter: FlagSet              = HasDefault | Param                             // A Scala 2x default parameter
  val DeferredInline: FlagSet                = Deferred | Inline
  val DeferredMethod: FlagSet                = Deferred | Method
  val DeferredOrLazy: FlagSet                = Deferred | Lazy
  val DeferredOrLazyOrMethod: FlagSet        = Deferred | Lazy | Method
  val DeferredOrTermParamOrAccessor: FlagSet = Deferred | ParamAccessor | TermParam           // term symbols without right-hand sides
  val DeferredOrTypeParam: FlagSet           = Deferred | TypeParam                           // type symbols without right-hand sides
  val EnumValue: FlagSet                     = Enum | StableRealizable                        // A Scala enum value
  val FinalOrInline: FlagSet                 = Final | Inline
  val FinalOrModuleClass: FlagSet            = Final | ModuleClass                            // A module class or a final class
  val ExcludedForwarder: Flags.FlagSet       = Specialized | Lifted | Protected | JavaStatic | PhantomSymbol
  val FinalOrSealed: FlagSet                 = Final | Sealed
  val InlineOrProxy: FlagSet                 = Inline | InlineProxy                           // An inline method or inline argument proxy */
  val InlineMethod: FlagSet                  = Inline | Method
  val InlineParam: FlagSet                   = Inline | Param
  val InlineByNameProxy: FlagSet             = InlineProxy | Method
  val JavaEnum: FlagSet                      = JavaDefined | Enum                             // A Java enum trait
  val JavaEnumValue: FlagSet                 = JavaDefined | EnumValue                        // A Java enum value
  val StaticProtected: FlagSet               = JavaDefined | JavaStatic | Protected           // Java symbol which is `protected` and `static`
  val JavaModule: FlagSet                    = JavaDefined | Module                           // A Java companion object
  val JavaInterface: FlagSet                 = JavaDefined | NoInits | Trait
  val JavaProtected: FlagSet                 = JavaDefined | Protected
  val MethodOrLazy: FlagSet                  = Lazy | Method
  val MethodOrLazyOrMutable: FlagSet         = Lazy | Method | Mutable
  val LiftedMethod: FlagSet                  = Lifted | Method
  val LocalParam: FlagSet                    = Local | Param
  val LocalParamAccessor: FlagSet            = Local | ParamAccessor
  val ProtectedLocal: FlagSet                = Local | Protected
  val MethodOrModule: FlagSet                = Method | Module
  val ParamForwarder: FlagSet                = Method | ParamAccessor | StableRealizable      // A parameter forwarder
  val StableMethod: FlagSet                  = Method | StableRealizable
  val NoInitsInterface: FlagSet              = NoInits | PureInterface
  val NoInitsTrait: FlagSet                  = NoInits | Trait                                // A trait that does not need to be initialized
  val ValidForeverFlags: FlagSet             = Package | Permanent
  val TermParamOrAccessor: FlagSet           = Param | ParamAccessor
  val SyntheticArtifact: FlagSet             = Synthetic | Artifact
  val SyntheticMethod: FlagSet               = Synthetic | Method
  val SyntheticModule: FlagSet               = Synthetic | Module
  val SyntheticParam: FlagSet                = Synthetic | Param
  val SyntheticTermParam: FlagSet            = Synthetic | TermParam
  val SyntheticTypeParam: FlagSet            = Synthetic | TypeParam
}
