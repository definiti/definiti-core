package definiti

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import definiti.api.{Core, DefinitionParser, TypeReference}
import definiti.generators.{ScalaGenerator, TypescriptGenerator}
import definiti.parser.{ASTValidation, Invalid, Parser, Valid}
import definiti.parser.antlr.{DefinitiLexer, DefinitiParser}
import org.antlr.v4.runtime._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

case class ErrorItem(line: Int, column: Int, msg: String)

/**
 * This listener can be used to use the real logger instead of simply print to the console.
 */
class ErrorListener extends BaseErrorListener {
  private val errors = ListBuffer[ErrorItem]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit =
    errors.append(ErrorItem(line, charPositionInLine, msg))

  def log(): Unit = errors.foreach(System.err.println)

  def hasError: Boolean = errors.nonEmpty
}

object Boot extends App {
  val source = Paths.get("src", "main", "resources", "samples", "first.def")
  val destination = Map(
    "scala" -> Paths.get("target", "samples", "result.scala"),
    "typescript" -> Paths.get("target", "samples", "result.typescript")
  )
  val rootDefinitionFiles = Paths.get("src", "main", "resources", "api")
  val definitionFiles = Files.find(rootDefinitionFiles, 1000, (path, _) => String.valueOf(path).endsWith(".definition"))

  val in = CharStreams.fromFileName("src/main/resources/samples/first.def")
  val lexer = new DefinitiLexer(in)
  val tokens = new CommonTokenStream(lexer)
  val parser = new DefinitiParser(tokens)

  val errorListener = new ErrorListener()
  parser.removeErrorListeners()
  parser.addErrorListener(errorListener)

  try {
    val result: DefinitiParser.DefinitiContext = parser.definiti()

    if (errorListener.hasError) {
      errorListener.log()
    } else {
      val ast = Parser.definitiContextToAST(result)

      definitionFiles.forEach { definitionFile =>
        val classDefinitions = DefinitionParser.parse(Files.readAllLines(definitionFile).asScala.mkString("", "\n", "\n"))
        classDefinitions.foreach(TypeReference.referenceType)
      }
      ast.verifications.foreach(TypeReference.referenceVerification)
      ast.classDefinitions.foreach(TypeReference.referenceType)
      ASTValidation.validate(ast) match {
        case Valid =>
          destination.foreach { case (language, path) =>
            def write(path: Path, str: String): Unit = {
              Files.createDirectories(path.getParent)
              Files.write(path, Seq(str).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
            }

            language match {
              case "scala" => write(path, ScalaGenerator.generate(ast))
              case "typescript" => write(path, TypescriptGenerator.generate(ast))
            }
          }
        case Invalid(errors) =>
          errors.foreach(System.err.println)
      }
    }
    println("done")
  } catch {
    // In some cases, an Exception is thrown because the parser do not recognize an expression and crash its tree.
    // Did not happened with a successful syntax yet.
    case e: Exception =>
      e.printStackTrace()
  }
}
