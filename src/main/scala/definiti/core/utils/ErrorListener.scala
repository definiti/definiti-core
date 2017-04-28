package definiti.core.utils

import org.antlr.v4.runtime._

import scala.collection.mutable.ListBuffer

case class ErrorItem(file: String, line: Int, column: Int, msg: String) {
  def prettyPrint: String = {
    s"""Error on file $file position $line-$column: $msg"""
  }
}

/**
 * This listener can be used to use the real logger instead of simply print to the console.
 */
class ErrorListener(source: String) extends BaseErrorListener {
  private val errorsBuffer = ListBuffer[ErrorItem]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit =
    errorsBuffer.append(ErrorItem(source, line, charPositionInLine, msg))

  def log(): Unit = errorsBuffer.foreach(System.err.println)

  def hasError: Boolean = errorsBuffer.nonEmpty

  def errors: Seq[ErrorItem] = errorsBuffer
}