package definiti.core.utils

import definiti.common.validation.{Error, SimpleError}
import org.antlr.v4.runtime._

import scala.collection.mutable.ListBuffer

private[core] case class ErrorItem(file: String, line: Int, column: Int, msg: String) {
  def prettyPrint: String = {
    s"""Error on file $file position $line-$column: $msg"""
  }

  def toError: Error = SimpleError(prettyPrint)
}

/**
 * This listener can be used to use the real logger instead of simply print to the console.
 */
private[core] class ErrorListener(source: String) extends BaseErrorListener {
  private val errorsBuffer = ListBuffer[ErrorItem]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit =
    errorsBuffer.append(ErrorItem(source, line, charPositionInLine, msg))

  def log(): Unit = errorsBuffer.foreach(System.err.println)

  def hasError: Boolean = errorsBuffer.nonEmpty

  def errors: Seq[ErrorItem] = errorsBuffer
}