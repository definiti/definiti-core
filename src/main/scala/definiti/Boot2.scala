package definiti

import definiti.parser.antlr.{DefinitiLexer, DefinitiParser}
import org.antlr.v4.runtime._

import scala.collection.mutable.ListBuffer

case class ErrorItem(line: Int, column: Int, msg: String)

/**
 * This listener can be used to use the real logger instead of simply print to the console.
 */
class ErrorListener extends BaseErrorListener {
  private val errors = ListBuffer[ErrorItem]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit =
    errors.append(ErrorItem(line, charPositionInLine, msg))

  def log() = errors.foreach(System.err.println)
}

object Boot2 extends App {
  val in = CharStreams.fromFileName("src/main/resources/samples/first.def")
  val lexer = new DefinitiLexer(in)
  val tokens = new CommonTokenStream(lexer)
  val parser = new DefinitiParser(tokens)

  val errorListener = new ErrorListener()
  parser.removeErrorListeners()
  parser.addErrorListener(errorListener)

  try {
    val result = parser.definiti()
    errorListener.log()
    println("done")
  } catch {
    // In some cases, an Exception is thrown because the parser do not recognize an expression and crash its tree.
    // Did not happened with a successful syntax yet.
    case _: Exception => println("error")
  }
}
