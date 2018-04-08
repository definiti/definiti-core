package definiti.core.parser

import definiti.core.utils.ErrorListener
import org.antlr.v4.runtime._

import scala.io.BufferedSource

private[parser] object ParserHelper {
  def buildParser[B <: Parser](source: String, newLexer: (CharStream) => Lexer, newParser: (TokenStream) => B, errorListener: ErrorListener): B = {
    buildParser(CharStreams.fromFileName(source), newLexer, newParser, errorListener)
  }

  def buildParser[B <: Parser](source: BufferedSource, newLexer: (CharStream) => Lexer, newParser: (TokenStream) => B, errorListener: ErrorListener): B = {
    buildParser(CharStreams.fromString(source.mkString), newLexer, newParser, errorListener)
  }

  def buildParser[B <: Parser](in: CharStream, newLexer: (CharStream) => Lexer, newParser: (TokenStream) => B, errorListener: ErrorListener): B = {
    val lexer = newLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = newParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    parser
  }
}
