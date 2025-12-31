/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast.symbols.classfile

object ClassfileConstants
{
  inline val JAVA_MAGIC             = 0xCAFEBABE
  inline val JAVA_MAJOR_VERSION     = 45
  inline val JAVA_MINOR_VERSION     = 3

  inline val JAVA_ACC_PUBLIC        = 0x0001
  inline val JAVA_ACC_PRIVATE       = 0x0002
  inline val JAVA_ACC_PROTECTED     = 0x0004
  inline val JAVA_ACC_STATIC        = 0x0008
  inline val JAVA_ACC_FINAL         = 0x0010
  inline val JAVA_ACC_SUPER         = 0x0020
  inline val JAVA_ACC_SYNCHRONIZED  = 0x0020
  inline val JAVA_ACC_VOLATILE      = 0x0040
  inline val JAVA_ACC_BRIDGE        = 0x0040
  inline val JAVA_ACC_TRANSIENT     = 0x0080
  inline val JAVA_ACC_NATIVE        = 0x0100
  inline val JAVA_ACC_INTERFACE     = 0x0200
  inline val JAVA_ACC_ABSTRACT      = 0x0400
  inline val JAVA_ACC_STRICT        = 0x0800
  inline val JAVA_ACC_SYNTHETIC     = 0x1000
  inline val JAVA_ACC_ANNOTATION    = 0x2000
  inline val JAVA_ACC_ENUM          = 0x4000

  inline val CONSTANT_UTF8          = 1
  inline val CONSTANT_UNICODE       = 2
  inline val CONSTANT_INTEGER       = 3
  inline val CONSTANT_FLOAT         = 4
  inline val CONSTANT_LONG          = 5
  inline val CONSTANT_DOUBLE        = 6
  inline val CONSTANT_CLASS         = 7
  inline val CONSTANT_STRING        = 8
  inline val CONSTANT_FIELDREF      = 9
  inline val CONSTANT_METHODREF     = 10
  inline val CONSTANT_INTFMETHODREF = 11
  inline val CONSTANT_NAMEANDTYPE   = 12

  inline val BYTE_TAG   = 'B'
  inline val CHAR_TAG   = 'C'
  inline val DOUBLE_TAG = 'D'
  inline val FLOAT_TAG  = 'F'
  inline val INT_TAG    = 'I'
  inline val LONG_TAG   = 'J'
  inline val SHORT_TAG  = 'S'
  inline val BOOL_TAG   = 'Z'
  inline val STRING_TAG = 's'
  inline val ENUM_TAG   = 'e'
  inline val CLASS_TAG  = 'c'
  inline val ARRAY_TAG  = '['
  inline val VOID_TAG   = 'V'
  inline val ANNOTATION_TAG = '@'
}
