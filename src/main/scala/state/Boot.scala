package state

import scala.collection.mutable.ListBuffer

object Boot extends App {
  var parts = ListBuffer[Syntax]()

  var acc: Syntax = Void
  Sample.testString.foreach { char =>
    (char, acc) match {
      case ('"', QuotedString(content)) if content.endsWith("\\") =>
        acc = QuotedString(content + char)
      case ('"', _: QuotedString) =>
        parts.append(acc)
        acc = Void
      case ('"', _) =>
        parts.append(acc)
        acc = QuotedString("")
      case (c, QuotedString(content)) =>
        acc = QuotedString(content + c)
      case ('\r' | '\n', EndOfLine) => // Do nothing
      case ('\r' | '\n', _) =>
        parts.append(acc)
        acc = EndOfLine
      case (c, Space) if c.isSpaceChar => // Do nothing
      case (c, _) if c.isSpaceChar =>
        parts.append(acc)
        acc = Space
      case ('{', _) =>
        parts.append(acc)
        acc = OpenBrace
      case ('}', _) =>
        parts.append(acc)
        acc = CloseBrace
      case ('(', _) =>
        parts.append(acc)
        acc = OpenParenthesis
      case (')', _) =>
        parts.append(acc)
        acc = CloseParenthesis
      case (':', _) =>
        parts.append(acc)
        acc = Colon
      case ('.', _) =>
        parts.append(acc)
        acc = Dot
      case ('_', _) =>
        parts.append(acc)
        acc = Underscore
      case (('=' | '>' | '<' | '+' | '-' | '/' | '*' | '%' | '|' | '&'), Symbol(content)) =>
        acc = Symbol(content + char)
      case (('=' | '>' | '<' | '+' | '-' | '/' | '*' | '%' | '|' | '&'), _) =>
        parts.append(acc)
        acc = Symbol(char.toString)
      case (c, Word(content)) if c.isLetterOrDigit =>
        acc = Word(content + char)
      case (c, _) if c.isLetterOrDigit =>
        parts.append(acc)
        acc = Word(char.toString)
      case _ =>
        acc = Unknown
    }
  }
  val cleanedParts = parts.filter(_ != Void)
  val partsWithKeywords = parts.map {
    case Word("verification") => VerificationKeyword
    case Word("verify") => VerifyKeyword
    case Word("verifying") => VerifyingKeyword
    case Word("type") => TypeKeyword
    case other => other
  }

  parts.foreach(println)
}