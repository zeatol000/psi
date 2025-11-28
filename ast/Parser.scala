/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2025       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psi.cc
package ast

import psi.cc.*
import psi.cc.utils.*
import psi.cc.ast.*
import psi.cc.ast.Tokens.*
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
//import scala.jdk.CollectionConverters.*
//import java.nio.file.{Files, Paths, Path}
//import java.io.FileInputStream


private[ast]
class Parser
(
  val path: String,
)
{
  inline val CR = 0x000D // Carriage Return -- \r
  inline val LF = 0x000A // Line Feed -- \n
  
  var sc: Scanner = null  /* Takes the Seq[Byte] and provides utilities to read
                             and tokenize the Seq[Byte] */
  var ast: AST = null     /* Provides a LOT */
  var pn: PatternN = null /* Tansforms people's weird syntax into normal syntax */
  var untpd: Untpd = null

  def parse(using Context): Unit = {
    vp"parsing $path"
    ast = ctxt.ast
    sc = new Scanner( File(path).read, path )
    ast ++ new Untpd( path )
    untpd = ast.untpd.getOrElse(path, null)

    while (sc.token ne EOF) {
      val stack = sc.stack
      var token = sc.token
      val start = sc.start

      given TD = TD(stack, token, start)

      sc.fetchToken()
      (sc.token eq IDENTIFIER, token eq IDENTIFIER) match {
        //case (false, true)  if isKeyword(sc.token)  => P(token, sc.stack)
        case (false, true)  if !isKeyword(sc.token)                             => Identifier
        case (false, false) => (sc.token, token) match {
          case (_, EMPTY)                                                       => void
          case (EMPTY, STRINGLIT) | (EMPTY, CHARLIT)                            => Token
          case (CHARLIT, CHARLIT) | (STRINGLIT, STRINGLIT)                      => void
          case (DOUBLELIT, _)                                                   => void
          case (_, INTLIT) if (sc.token ne INTLIT)
            && (!stack.contains('.'))                                           => Token
          //case (_, INTLIT) if (sc.token ne INTLIT)
          //  && (stack.contains('.'))                         => token = DOUBLELIT; P(token, stack, start)
          case (INTLIT, INTLIT)                                                 => void
          case (_, _)                                                           => Token
        }
        case (true, false)                                                      => Token
        case (true, true) if ((stack.length eq sc.stack.length))                => Identifier
        case (true, true) if !sc.stack.startsWith(stack)                        => Identifier
        case (_, _)                                                             => void
      }
    }
    ast << untpd
    println(s"""
      |Identifiers:
      |${untpd.dumpId}
      |
      |
      |Tags:
      |${untpd.dumpAst}
      |""".stripMargin
    )
  }
    // TODO:
    // - Finish keywords and punctuation handling
    // - Allow for types to have internal types. Eg: Array[ String ]
    // - Handle SELECT tags
    // - Create handling for pApply -- either NameRef, APPLY, or SELECT
    // - Push AnyVal into exact values instead of NameRef, which AnyRef is.
    // - ast: push the syntax made by this into an internal untpd class,
    //   write it to a file, and keep track of all untpd files.
    //
    // the scanner was so good, just for the parser to be a nightname (wilted flower emoji)
    //
    // HINT:
    // to convert hex or binary to integers, take all of the values in the stack
    // and merge them into a string, then run "Integer.parseInt(a, b)"
    // such that a is the string and b is the radix
  


  var loopDepth: Byte = 0 // what kind of instane bastard would use 128 nested blocks
  def lookingForId: Boolean = ((idType != ' ') && (idType != '?'))
  var idType: Char = ' '                    // type of identifier -- [n]ame, [:]type, [=]value
  var modifierStack = mutable.Seq[Byte]()   // stack of modifiers
  var inParam: Boolean = false
  var paramType: Char = ' '                 // type of param -- () [] <>
  var ApplyStack = mutable.Seq[Byte]()

  def Identifier(using td: TD)(using Context): Unit = {  
    if (!lookingForId && idType != 's') /* handle APPLY and SELECT */ {
      ApplyStack = td.stack
      idType = '?'
      untpd << untpd.opBuf
      return
    }

    if (td.token == NEW && idType != 'n') {
      untpd operate new New
      idType = 'n'
      return
    }

    untpd.opBuf match {
      case v: VALDEF          if !inParam =>
        untpd addId td.stack
        idType match {
          case 'n' => v.name = untpd indexOf td.stack
          case ':' => v.valType = untpd indexOf td.stack
          case '=' => v.value = (td.token.toByte, untpd indexOf td.stack)
        }

      case f: FNDEF           if !inParam =>
        untpd addId td.stack
        idType match {
          case 'n' => f.name = untpd indexOf td.stack
          case ':' => f.valType = untpd indexOf td.stack
          case '=' => untpd << f
        }

      case o: OPDEF           if !inParam =>
        untpd addId td.stack
        idType match {
          case 'n' => o.name = untpd indexOf td.stack
          case ':' => o.valType = untpd indexOf td.stack
          case '=' => untpd << o
        }

      case c: CLASSDEF        if !inParam =>
        untpd addId td.stack
        idType match {
          case 'n' => c.name = untpd indexOf td.stack
          case '=' => untpd << c
        }

      case o: OBJDEF          if !inParam =>
        untpd addId td.stack
        idType match {
          case 'n' => o.name = untpd indexOf td.stack
          case '=' => untpd << o
        }

      case a: APPLY           if !inParam =>
        untpd addId td.stack
        a.name = untpd indexOf td.stack

      case _                  if !inParam => vp"null id: ${td.stack.map(_.toChar).mkString("")}"
        

      case _                  if  inParam =>
        td.token match {
          case RPAREN   if paramType == '(' => inParam = false; paramType = ' '; idType = ' '; return
          case RBRACKET if paramType == '[' => inParam = false; paramType = ' '; idType = ' '; return
          case RSHARP   if paramType == '{' => inParam = false; paramType = ' '; idType = ' '; return
          case _ =>
        }

        untpd.paramBuf match {
          case c: pCreate =>
            idType match {

              case 'n' | ' ' | '?' =>
                untpd addId td.stack
                paramType match {
                  case '(' =>
                    val end = c.paren.length - 1
                    c.paren(end) = (
                      untpd indexOf td.stack,
                      c.paren(end)._2,
                      c.paren(end)._3
                    )
                  case '[' => 
                    val end = c.brack.length - 1
                    c.brack(end) = (
                      untpd indexOf td.stack,
                      c.brack(end)._2,
                      c.brack(end)._3
                    )
                  case '<' => 
                    val end = c.sharp.length - 1
                    c.sharp(end) = (
                      untpd indexOf td.stack,
                      c.sharp(end)._2,
                      c.sharp(end)._3
                    )
              }

              case ':' =>
                untpd addId td.stack
                paramType match {
                  case '(' =>
                    val end = c.paren.length - 1
                    c.paren(end) = (
                      c.paren(end)._1,
                      untpd indexOf td.stack,
                      c.paren(end)._3
                    )
                  case '[' => 
                    val end = c.brack.length - 1
                    c.brack(end) = (
                      c.brack(end)._1,
                      untpd indexOf td.stack,
                      c.brack(end)._3
                    )
                  case '<' => 
                    val end = c.sharp.length - 1
                    c.sharp(end) = (
                      c.sharp(end)._1,
                      untpd indexOf td.stack,
                      c.sharp(end)._3
                    )
                }

              case '=' =>
                if (td.token == IDENTIFIER || td.token == STRINGLIT) {
                  untpd addId td.stack
                  paramType match {
                  case '(' =>
                    val end = c.paren.length - 1
                    c.paren(end) = (
                      c.paren(end)._1,
                      c.paren(end)._2,
                      untpd indexOf td.stack,
                    )
                  case '[' => 
                    val end = c.brack.length - 1
                    c.brack(end) = (
                      c.brack(end)._1,
                      c.brack(end)._2,
                      untpd indexOf td.stack,
                    )
                  case '<' => 
                    val end = c.sharp.length - 1
                    c.sharp(end) = (
                      c.sharp(end)._1,
                      c.sharp(end)._2,
                      untpd indexOf td.stack,
                    )
                  }
                }
            }
          case a: pApply =>
            untpd addId td.stack
            paramType match {
              case '(' =>
                val end = a.paren.length - 1
                // TODO: add complex if statement handling here. Such as ==, !=, return of an APPLY, etc.
                // We can do this by treating all conditions as a method of ANY, which means we need to
                // figure out SELECT with APPLY.... For now, just throw a (token, NameRef)
                a.paren(end) = (td.token.toByte, untpd indexOf td.stack)
              case '[' =>
                val end = a.brack.length - 1
                a.brack(end) = (td.token.toByte, untpd indexOf td.stack)
              case '<' =>
                val end = a.sharp.length - 1
                a.sharp(end) = (td.token.toByte, untpd indexOf td.stack)
            }
          case _ => println("FUCK")
        }
      /*case _ =>
        idType match {

          case ':' =>
            untpd addId td.stack
            

          case '=' =>
            untpd addId td.stack
            paramType match {

            }

        }*/
    }
    idType = ' '
  }



  def Token(using td: TD)(using Context): Unit = { 
    if (lookingForId) { Identifier; return } // force token to be an identifier

    if (isDeclare) td.token match {
      case VAL  =>
        untpd operate new VALDEF( modifierStack )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case VAR  =>
        untpd operate new VALDEF( modifierStack :+ VAR )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case PRO  =>
        untpd operate new FNDEF( modifierStack :+ PRO, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case SUB  =>
        untpd operate new FNDEF( modifierStack :+ SUB, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case CO   =>
        untpd operate new FNDEF( modifierStack :+ CO, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case FN   =>
        untpd operate new FNDEF( modifierStack, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
       

      case OP   =>
        untpd operate new OPDEF( modifierStack, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'

      case TYPE =>
        untpd operate new VALDEF ( modifierStack :+ TYPE )
        modifierStack = mutable.Seq()
        idType = 'n'
        
        


    } else if (isDefine) td.token match {
      case OBJ    =>
        untpd operate new OBJDEF ( modifierStack, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case CLASS  =>
        untpd operate new CLASSDEF ( modifierStack, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case TRAIT  =>
        untpd operate new CLASSDEF ( modifierStack :+ TRAIT, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
        

      case MOD    => // this probably needs it's own MODDEF token, but lets wait until 1.0.0
        untpd operate new OBJDEF ( modifierStack :+ MOD, startDepth = loopDepth )
        modifierStack = mutable.Seq()
        idType = 'n'
        



    } else if (isModifier) { modifierStack = modifierStack :+ td.token.toByte }



    else if (isParen) td.token match {
      case LPAREN =>
        if (idType == '?') {
          untpd operate new APPLY
          untpd addId ApplyStack
          untpd.opBuf match { case a: APPLY => a.name = untpd indexOf ApplyStack; case _ => }
          ApplyStack = mutable.Seq()
        }
        untpd.opBuf match {
          case f: FNDEF => untpd <> new pCreate
          case o: OPDEF => untpd <> new pCreate
          case a: APPLY => untpd <> new pApply
          case i: If    => untpd <> new pApply
          case w: While => untpd <> new pApply
          case n: New   => untpd <> new pApply
          case _ => Error(s"tag ${untpd.opBuf} does not take parameters", path)
        }
        idType = 'n'
        inParam = true
        paramType = '('

      case LBRACKET =>
         if (idType == '?') {
          untpd operate new APPLY
          untpd addId ApplyStack
          untpd.opBuf match { case a: APPLY => a.name = untpd indexOf ApplyStack; case _ => }
          ApplyStack = mutable.Seq()
        }     
        untpd.opBuf match {
          case f: FNDEF => untpd <> new pCreate
          case o: OPDEF => untpd <> new pCreate
          case a: APPLY => untpd <> new pApply
          case i: If    => untpd <> new pApply
          case w: While => untpd <> new pApply
          case n: New   => untpd <> new pApply
          case _ => Error(s"tag ${untpd.opBuf} does not take parameters", path)
        } 
        inParam = true
        idType = ':'
        paramType = '['

      case LSHARP =>
         if (idType == '?') {
          untpd operate new APPLY
          untpd addId ApplyStack
          untpd.opBuf match { case a: APPLY => a.name = untpd indexOf ApplyStack; case _ => }
          ApplyStack = mutable.Seq()
        }
        untpd.opBuf match {
          case f: FNDEF => untpd <> new pCreate
          case o: OPDEF => untpd <> new pCreate
          case a: APPLY => untpd <> new pApply
          case i: If    => untpd <> new pApply
          case w: While => untpd <> new pApply
          case n: New   => untpd <> new pApply
          case _ => Error(s"tag ${untpd.opBuf} does not take parameters", path)
        }
        inParam = true
        idType = 'n'
        paramType = '<'

      case LBRACE =>
        loopDepth = (loopDepth + 1).toByte
        paramType = ' '

      case RPAREN | RBRACKET | RSHARP =>
        inParam = false
        paramType = ' '
        if (td.token == RBRACKET) idType = ' '
        if (untpd.paramBuf != null) untpd.opBuf match {
          case f: FNDEF    => untpd.paramBuf match { case x: pCreate => f.params = f.params :+ x }
          case o: OPDEF    => untpd.paramBuf match { case x: pCreate => o.params = o.params :+ x }
          case a: APPLY    => untpd.paramBuf match { case x: pApply  => a.params = a.params :+ x }
          case i: If       => untpd.paramBuf match { case x: pApply  => i.cond   = i.cond   :+ x }
          case null        => void
          case _ =>
            Error(s"tag ${untpd.opBuf} does not take parameters", path)
        }
        untpd.paramBuf = null

      case RBRACE =>
        loopDepth = (loopDepth - 1).toByte
        paramType = ' '
        var i = untpd.nestBuf.length - 1
        if (untpd.opBuf == null) return
        untpd << untpd.opBuf
        breakable { while (i >= 0) {
          untpd.nestBuf(i) match {
            case f: FNDEF    if f.open => f.open = false; f.body = f.body :+ untpd.opBuf; break
            case o: OPDEF    if o.open => o.open = false; o.body = o.body :+ untpd.opBuf; break
            case c: CLASSDEF if c.open => c.open = false; c.body = c.body :+ untpd.opBuf; break
            case o: OBJDEF   if o.open => o.open = false; o.body = o.body :+ untpd.opBuf; break
            case i: If       if i.open => i.open = false; i.body = i.body :+ untpd.opBuf; break
            case _ => i = i - 1
          }
        }}
        untpd.opBuf = null


    } else if (isPunctuation) td.token match {
      //case DOT =>
      //  idType = 's'
        
      case COMMA if inParam =>
        idType = 'n'
        paramType match {
          case '(' => untpd.paramBuf match {
            case c: pCreate => c.paren = c.paren :+ (0,0,null)
            case a: pApply  => a.paren = a.paren :+ (1,0) }
          case '[' => untpd.paramBuf match {
            case c: pCreate => c.brack = c.brack :+ (0,0,null)
            case a: pApply  => a.brack = a.brack :+ (1,0) }
          case '<' => untpd.paramBuf match {
            case c: pCreate => c.sharp = c.sharp :+ (0,0,null)
            case a: pApply  => a.sharp = a.sharp :+ (1,0) }
        }

      case COMMA if !inParam => void

      // TODO:
      case SEMI     => void // i have no clue what to do with semicolons
      //case USCORE   => void // undefined value
      //case ASTERISK => void // all values 
      //case TILDE    => void // idk

      case EQUALS =>
        if (inParam) idType = '='
        else untpd.opBuf match {
          case v: VALDEF => idType = '='
          case s: SELECT => idType = '='
          case _ => void
        }

      case COLON =>
        idType = ':'

      // arrow handling ..

      case _ =>
        Error(s"token ${tokenStr(td.token)} isn't implemented yet.. Oopsie", path)
        // TODO: add the other punctuation

    } else if (isKeyword(td.token)) td.token match {
      case IF      => untpd operate new If
      case ELSE    =>
        var i = untpd.nestBuf.length - 1
        breakable { while (i >= 0) {
          untpd.nestBuf(i) match {
            case i: If if !i.open => i.ifBuf = i.body; i.open = true; i.body = mutable.Seq(); untpd.opBuf = null; break
            case _ => i = i - 1
          }
        }}
      //case FOR     => // FOR is stupid
      case WHILE   => untpd operate new While
      case NEW     => untpd operate new New
      //case THIS    =>
      //case SUPER   =>
      //case MATCH   => untpd operate new Match
      //case CASE    => untpd operate new Case
      //case TRY     => untpd operate new Try
      //case CATCH   => untpd.opBuf match {case t: Try => t.catchOpen = true}
      //case FINALLY =>
      case RETURN  => untpd operate new Return
      //case THROW   => untpd operate new Throw
      case IMPORT  => untpd operate new Import
      case PACKAGE => untpd operate new PACKAGEDEF
      
    }

  }




  inline def P(using td: TD): Unit =
    val t = td.token
    val start = td.start
    val s = td.stack
    if (t <= 15) println(s"(${t}) ${tokenStr(t)} | (${start}) ${s.map(_.toChar).mkString("")}")
    else println(s"(${t}) ${tokenStr(t)}  \t$start ")

  // classifying tokens ///////////////////////////////////////////////////////
  def isPunctuation(using td: TD): Boolean = (
    (td.token == DOT)      ||
    (td.token == COMMA)    ||
    (td.token == SEMI)     ||
    (td.token == USCORE)   ||
    (td.token == ASTERISK) ||
    (td.token == TILDE)    ||
    //(td.token == NEWLINE)  ||
    (td.token == EQUALS)   ||
    (td.token == COLON)    ||
    //(td.token == DOUBLECOLON) ||
    (td.token == LARRS)    ||
    //(td.token == LARRB)    ||
    (td.token == RARRS)    ||
    (td.token == RARRB)    )
  def isParen(using td: TD): Boolean = (
    (td.token == LPAREN)   ||
    (td.token == RPAREN)   ||
    (td.token == LBRACKET) ||
    (td.token == RBRACKET) ||
    (td.token == LBRACE)   ||
    (td.token == RBRACE)   ||
    (td.token == LSHARP)   ||
    (td.token == RSHARP)   )
  def isModifier(using td: TD): Boolean = (
    (td.token == ABSTRACT)  ||
    (td.token == FINAL)     ||
    (td.token == PRIVATE)   ||
    (td.token == INHERITED) ||
    (td.token == OVERRIDE)  )
  def isDefine(using td: TD): Boolean = (
    (td.token == OBJ)   ||
    (td.token == CLASS) ||
    (td.token == TRAIT) ||
    (td.token == MOD)   )
  def isDeclare(using td: TD): Boolean = (
    (td.token == VAL) ||
    (td.token == VAR) ||
    (td.token == PRO) ||
    (td.token == SUB) ||
    (td.token == CO)  ||
    (td.token == FN)  ||
    (td.token == OP)  ||
    (td.token == TYPE))
  def isExpressionStart(using td: TD): Boolean = (
    (td.token == IDENTIFIER)  ||
    (td.token == CHARLIT)     ||
    (td.token == INTLIT)      ||
    (td.token == LONGLIT)     ||
    (td.token == FLOATLIT)    ||
    (td.token == DOUBLELIT)   ||
    (td.token == STRINGLIT)   ||
    (td.token == NULL)        ||
    (td.token == TRUE)        ||
    (td.token == FALSE)       ||
    (td.token == THIS)        ||
    (td.token == SUPER)       ||
    (td.token == NEW)         ||
    (td.token == IF)          ||
    (td.token == FOR)         ||
    (td.token == TRY)         ||
    (td.token == WHILE)       ||
    (td.token == RETURN)      ||
    (td.token == THROW)       ||
    (td.token == LPAREN)      ||
    (td.token == LBRACE)      )
}
