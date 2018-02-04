package definiti.core.parser

import definiti.core.utils.ErrorListener
import org.antlr.v4.runtime._

private[parser] object ParserHelper {
  def buildParser[B <: Parser](source: String, newLexer: (CharStream) => Lexer, newParser: (TokenStream) => B, errorListener: ErrorListener): B = {
    val in = CharStreams.fromFileName(source)
    val lexer = newLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = newParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    parser
  }
}
