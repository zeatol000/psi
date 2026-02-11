/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              */

package psic.tools
package comp.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.unsafeNulls

object DiffUtil {
  val EOF: String = "EOF"
  val ansiToken: Char = '\u001b'

  @tailrec private def splitTokens(str: String, acc: List[String] = Nil): List[String] =
    if str == "" then acc.reverse
    else {
      val head = str.charAt(0)
      val (token, rest) =
        if head == ansiToken then
          val index = str.indexOf('m') + 1
          (str.substring(0, index), str.substring(index))

        else if Character.isAlphabetic(head) || Character.isDigit(head) then
          str.span: c =>
            Character.isAlphabetic(c) || Character.isDigit(c) && c != ansiToken

        else if Character.isMirrored(head) || Character.isWhitespace(head) then
          str.splitAt(1)

        else str.span: c =>
          !Character.isAlphabetic(c) && !Character.isDigit(c)      &&
          !Character.isMirrored(c)   && !Character.isWhitespace(c) &&
          c != ansiToken

      splitTokens(rest, token :: acc)
    }

  def mkColoredTypeDiff(found: String, expect: String): (String, String, Double) = {
    var change = 0
    val foundTk = splitTokens(found).toArray
    val expectedTk = splitTokens(expect).toArray

    val diffexpect = hirschberg(foundTk, expectedTk)
    val diffactual = hirschberg(expectedTk, foundTk)

    val exp = diffexpect.collect {
      case Unmodified(str) => str
      case Inserted(str) => change += str.length; added(str)
    }.mkString

    val fnd = diffactual.collect {
      case Unmodified(str) => str
      case Inserted(str) => change += str.length; deleted(str)
    }.mkString

    val fin = change.toDouble / (expect.length + found.length)

    (fnd, exp, fin)
  }

  def mkColoredLineDiff(expected: Seq[String], actual: Seq[String]): String = {
    val longestExpected = expected.map(_.length).maxOption.getOrElse(0)
    val longestActual = actual.map(_.length).maxOption.getOrElse(0)
    val expectedSize = EOF.length max longestActual max longestExpected
    actual.padTo(expected.length, "").zip(expected.padTo(actual.length, "")).map { case (act, exp) =>
      mkColoredLineDiff(exp, act, expectedSize)
    }.mkString(System.lineSeparator)
  }

  def mkColoredLineDiff(expected: String, actual: String, expectedSize: Int): String = {
    lazy val diff = {
      val tokens = splitTokens(expected, Nil).toArray
      val lastTokens = splitTokens(actual, Nil).toArray
      hirschberg(lastTokens, tokens)
    }

    val expectedDiff =
      if (expected eq EOF) eof()
      else diff.collect {
        case Unmodified(str) => str
        case Inserted(str) => added(str)
        case Modified(_, str) => added(str)
        case Deleted(_) => ""
      }.mkString

    val actualDiff =
      if (actual eq EOF) eof()
      else diff.collect {
        case Unmodified(str) => str
        case Inserted(_) => ""
        case Modified(str, _) => deleted(str)
        case Deleted(str) => deleted(str)
      }.mkString

    val pad = " " * 0.max(expectedSize - expected.length)

    expectedDiff + pad + "  |  " + actualDiff
  }

  private def ensureLineSeparator(str: String): String =
    if str.endsWith(System.lineSeparator) then
      str
    else
      str + System.lineSeparator


  def mkColoredHorizontalLineDiff(expected: String, actual: String): String = {
    val indent = 2
    val tab = " " * indent
    val insertIndent = "+" ++ (" " * (indent - 1))
    val deleteIndent = "-" ++ (" " * (indent - 1))

    if actual.isEmpty then
      (expected.linesIterator.map(line => added(insertIndent + line)).toList :+ deleted("--- EMPTY OUTPUT ---"))
        .map(ensureLineSeparator).mkString
    else if expected.isEmpty then
      (added("--- NO VALUE EXPECTED ---") +: actual.linesIterator.map(line => deleted(deleteIndent + line)).toList)
        .map(ensureLineSeparator).mkString
    else
      lazy val diff = {
        val expectedTokens = expected.linesWithSeparators.toArray
        val actualTokens = actual.linesWithSeparators.toArray
        hirschberg(actualTokens, expectedTokens)
      }.toList

      val transformedDiff = diff.flatMap {
        case Modified(original, str) => Seq(
          Inserted(ensureLineSeparator(original)), Deleted(ensureLineSeparator(str))
        )
        case other => Seq(other)
      }

      val zipped = transformedDiff zip transformedDiff.drop(1)

      val (acc, inserts, deletions) = zipped.foldLeft((Seq[Patch](), Seq[Inserted](), Seq[Deleted]())): (acc, patches) =>
        val (currAcc, inserts, deletions) = acc
        patches match
          case (currentPatch: Inserted, nextPatch: Deleted) =>
            (currAcc, inserts :+ currentPatch, deletions)
          case (currentPatch: Deleted, nextPatch: Inserted) =>
            (currAcc, inserts, deletions :+ currentPatch)
          case (currentPatch, nextPatch) =>
            (currAcc :++ inserts :++ deletions :+ currentPatch, Seq.empty, Seq.empty)

      val stackedDiff = acc :++ inserts :++ deletions :+ diff.last

      stackedDiff.collect {
        case Unmodified(str) => tab + str
        case Inserted(str) => added(insertIndent + str)
        case Deleted(str) => deleted(deleteIndent + str)
      }.map(ensureLineSeparator).mkString

  }

  def mkColoredCodeDiff(code: String, lastCode: String, printDiffDel: Boolean): String = {
    val tokens = splitTokens(code, Nil).toArray
    val lastTokens = splitTokens(lastCode, Nil).toArray

    val diff = hirschberg(lastTokens, tokens)

    diff.collect {
      case Unmodified(str) => str
      case Inserted(str)                      => added(str)
      case Modified(old, str) if printDiffDel => deleted(str) + added(str)
      case Modified(_, str)                   => added(str)
      case Deleted(str) if printDiffDel       => deleted(str)
    }.mkString
  }

  private def added(str: String): String = bgColored(str, Console.GREEN)
  private def deleted(str: String) = bgColored(str, Console.RED)
  private def bgColored(str: String, color: String): String =
    if (str.isEmpty) ""
    else {
      val (spaces, rest) = str.span(_ == '\n')
      if (spaces.isEmpty) {
        val (text, rest2) = str.span(_ != '\n')
        Console.BOLD + color + text + Console.RESET + bgColored(rest2, color)
      }
      else spaces + bgColored(rest, color)
    }
  private def eof() = "\u001B[51m" + "EOF" + Console.RESET

  private sealed trait Patch
  private final case class Unmodified(str: String) extends Patch
  private final case class Modified(original: String, str: String) extends Patch
  private final case class Deleted(str: String) extends Patch
  private final case class Inserted(str: String) extends Patch

  private def hirschberg(a: Array[String], b: Array[String]): Array[Patch] = {
    def build(x: Array[String], y: Array[String], builder: mutable.ArrayBuilder[Patch]): Unit =
      if (x.isEmpty)
        builder += Inserted(y.mkString)
      else if (y.isEmpty)
        builder += Deleted(x.mkString)
      else if (x.length == 1 || y.length == 1)
        needlemanWunsch(x, y, builder)
      else {
        val xlen = x.length
        val xmid = xlen / 2
        val ylen = y.length

        val (x1, x2) = x.splitAt(xmid)
        val leftScore = nwScore(x1, y)
        val rightScore = nwScore(x2.reverse, y.reverse)
        val scoreSum = (leftScore zip rightScore.reverse).map {
          case (left, right) => left + right
        }
        val max = scoreSum.max
        val ymid = scoreSum.indexOf(max)

        val (y1, y2) = y.splitAt(ymid)
        build(x1, y1, builder)
        build(x2, y2, builder)
      }
    val builder = Array.newBuilder[Patch]
    build(a, b, builder)
    builder.result()
  }

  private def nwScore(x: Array[String], y: Array[String]): Array[Int] = {
    def ins(s: String) = -2
    def del(s: String) = -2
    def sub(s1: String, s2: String) = if (s1 == s2) 2 else -1

    val score = Array.fill(x.length + 1, y.length + 1)(0)
    for (j <- 1 to y.length)
      score(0)(j) = score(0)(j - 1) + ins(y(j - 1))
    for (i <- 1 to x.length) {
      score(i)(0) = score(i - 1)(0) + del(x(i - 1))
      for (j <- 1 to y.length) {
        val scoreSub = score(i - 1)(j - 1) + sub(x(i - 1), y(j - 1))
        val scoreDel = score(i - 1)(j) + del(x(i - 1))
        val scoreIns = score(i)(j - 1) + ins(y(j - 1))
        score(i)(j) = scoreSub max scoreDel max scoreIns
      }
    }
    Array.tabulate(y.length + 1)(j => score(x.length)(j))
  }

  private def needlemanWunsch(x: Array[String], y: Array[String], builder: mutable.ArrayBuilder[Patch]): Unit = {
    def similarity(a: String, b: String) = if (a == b) 3 else -1
    val d = 1
    val score = Array.tabulate(x.length + 1, y.length + 1) { (i, j) =>
      if (i == 0) d * j
      else if (j == 0) d * i
      else 0
    }
    for (i <- 1 to x.length)
      for (j <- 1 to y.length) {
        val mtch = score(i - 1)(j - 1) + similarity(x(i - 1), y(j - 1))
        val delete = score(i - 1)(j) + d
        val insert = score(i)(j - 1) + d
        score(i)(j) = mtch max insert max delete
      }

    var alignment = List.empty[Patch]
    var i = x.length
    var j = y.length
    while (i > 0 || j > 0)
      if (i > 0 && j > 0 && score(i)(j) == score(i - 1)(j - 1) + similarity(x(i - 1), y(j - 1))) {
        val newHead =
          if (x(i - 1) == y(j - 1)) Unmodified(x(i - 1))
          else Modified(x(i - 1), y(j - 1))
        alignment = newHead :: alignment
        i = i - 1
        j = j - 1
      }
      else if (i > 0 && score(i)(j) == score(i - 1)(j) + d) {
        alignment = Deleted(x(i - 1)) :: alignment
        i = i - 1
      }
      else {
        alignment = Inserted(y(j - 1)) :: alignment
        j = j - 1
      }
    builder ++= alignment
  }
}
