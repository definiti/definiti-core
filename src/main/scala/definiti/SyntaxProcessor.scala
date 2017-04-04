package definiti

import scala.collection.mutable.ListBuffer

object SyntaxProcessor {
  def processString(source: String): Seq[SyntaxToken] = {
    val parts = ListBuffer[SyntaxToken]()

    var acc: SyntaxToken = Void
    source.foreach { char =>
      (char, acc) match {
        case ('\r', _) => // Ignore
        case ('"', QuotedString(content)) if content.endsWith("\\") =>
          acc = QuotedString(content + char)
        case ('"', _: QuotedString) =>
          parts.append(acc)
          acc = Void
        case ('"', x) if !(x.isInstanceOf[BlockComment] || x.isInstanceOf[LineComment])  =>
          parts.append(acc)
          acc = QuotedString("")
        case (c, QuotedString(content)) =>
          acc = QuotedString(content + c)
        case ('*', SymbolString("/")) =>
          acc = BlockComment("")
        case ('/', BlockComment(content)) if content.endsWith("*") =>
          parts.append(BlockComment(content.substring(0, content.length - 1)))
          acc = Void
        case (c, BlockComment(content)) =>
          acc = BlockComment(content + c)
        case ('\n', EndOfLine) => // Do nothing
        case ('\n', _) =>
          parts.append(acc)
          acc = EndOfLine
        case ('/', SymbolString("/")) =>
          acc = LineComment("")
        case (c, LineComment(content)) =>
          acc = LineComment(content + c)
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
        case (('=' | '>' | '<' | '+' | '-' | '/' | '*' | '%' | '|' | '&'), SymbolString(content)) =>
          acc = SymbolString(content + char)
        case (('=' | '>' | '<' | '+' | '-' | '/' | '*' | '%' | '|' | '&'), _) =>
          parts.append(acc)
          acc = SymbolString(char.toString)
        case (c, Word(content)) if c.isLetterOrDigit =>
          acc = Word(content + char)
        case (c, _) if c.isLetterOrDigit =>
          parts.append(acc)
          acc = Word(char.toString)
        case _ =>
          acc = Unknown
      }
    }
    parts.append(acc)

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
        case SymbolString("=") => AssignSymbol
        case SymbolString("==") => EqualSymbol
        case SymbolString("!") => NotSymbol
        case SymbolString("!=") => NotEqualSymbol
        case SymbolString("<") => LowerSymbol
        case SymbolString(">") => UpperSymbol
        case SymbolString("<=") => LowerOrEqualSymbol
        case SymbolString(">=") => UpperOrEqualSymbol
        case SymbolString("=>") => MapSymbol
        case SymbolString("&&") => AndSymbol
        case SymbolString("||") => OrSymbol
        case SymbolString("+") => PlusSymbol
        case SymbolString("-") => MinusSymbol
        case SymbolString("*") => TimeSymbol
        case SymbolString("/") => DivideSymbol
        case SymbolString("%") => ModuloSymbol
        case other => other
      }
      .foldLeft(List[SyntaxToken]()) { case (resultAcc, token) =>
        if (token == EndOfLine && resultAcc.lastOption.contains(EndOfLine)) {
          resultAcc
        } else {
          resultAcc :+ token
        }
      }
  }
}
