package state

import scala.collection.mutable.ListBuffer

object SyntaxProcessor {
  def processString(source: String): Seq[SyntaxToken] = {
    val parts = ListBuffer[SyntaxToken]()

    var acc: SyntaxToken = Void
    source.foreach { char =>
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
        case (',', _) =>
          parts.append(acc)
          acc = Comma
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

    parts
      .view
      .filter(_ != Void)
      .filter(_ != Space)
      .map {
        case Word("verification") => VerificationKeyword
        case Word("verify") => VerifyKeyword
        case Word("verifying") => VerifyingKeyword
        case Word("type") => TypeKeyword
        case Word("if") => IfKeyword
        case Word("else") => ElseKeyword
        case Word("true") => TrueKeyword
        case Word("false") => FalseKeyword
        case Symbol("=") => AssignSymbol
        case Symbol("==") => EqualSymbol
        case Symbol("!") => NotSymbol
        case Symbol("!=") => NotEqualSymbol
        case Symbol("<") => LowerSymbol
        case Symbol(">") => UpperSymbol
        case Symbol("<=") => LowerOrEqualSymbol
        case Symbol(">=") => UpperOrEqualSymbol
        case Symbol("=>") => MapSymbol
        case Symbol("&&") => AndSymbol
        case Symbol("||") => OrSymbol
        case Symbol("+") => PlusSymbol
        case Symbol("-") => MinusSymbol
        case Symbol("*") => TimeSymbol
        case Symbol("/") => DivideSymbol
        case Symbol("%") => ModuloSymbol
        case other => other
      }
      .force
  }
}
