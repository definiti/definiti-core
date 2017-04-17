package definiti

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import definiti.api.ReferenceContext
import definiti.generators.{ScalaGenerator, TypescriptGenerator}
import definiti.parser._
import definiti.parser.antlr.{CoreDefinitionLexer, CoreDefinitionParser, DefinitiLexer, DefinitiParser}
import org.antlr.v4.runtime._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

case class ErrorItem(line: Int, column: Int, msg: String)

/**
 * This listener can be used to use the real logger instead of simply print to the console.
 */
class ErrorListener extends BaseErrorListener {
  private val errorsBuffer = ListBuffer[ErrorItem]()

  override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit =
    errorsBuffer.append(ErrorItem(line, charPositionInLine, msg))

  def log(): Unit = errorsBuffer.foreach(System.err.println)

  def hasError: Boolean = errorsBuffer.nonEmpty

  def errors: Seq[ErrorItem] = errorsBuffer
}

object Boot extends App {
  val source = Paths.get("src", "main", "resources", "samples", "first.def")
  val destination = Map(
    "scala" -> Paths.get("target", "samples", "result.scala"),
    "typescript" -> Paths.get("target", "samples", "result.typescript")
  )
  val rootDefinitionFiles = Paths.get("src", "main", "resources", "api")
  val definitionFiles = Files.find(rootDefinitionFiles, 1000, (path, _) => String.valueOf(path).endsWith(".definition"))

  try {
    parseDefinitiFile(source.toString) match {
      case Left(errors) => errors.foreach(System.err.println)
      case Right(ast) =>
        val typeBuffer = ListBuffer[ClassDefinition]()
        val verificationBuffer = ListBuffer[Verification]()
        definitionFiles.forEach { definitionFile =>
          parseCoreDefinitionFile(definitionFile.toString) match {
            case Left(errors) => errors.foreach(System.err.println)
            case Right(classDefinitions) => classDefinitions.foreach(typeBuffer.append(_))
          }
        }

        verificationBuffer.append(ast.verifications: _*)
        typeBuffer.append(ast.classDefinitions: _*)

        implicit val context = ReferenceContext(typeBuffer, verificationBuffer)
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

  def parseDefinitiFile(source: String): Either[Seq[ErrorItem], Root] = {
    val in = CharStreams.fromFileName(source)
    val lexer = new DefinitiLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = new DefinitiParser(tokens)
    val errorListener = new ErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    val result: DefinitiParser.DefinitiContext = parser.definiti()
    if (errorListener.hasError) {
      Left(errorListener.errors)
    } else {
      Right(DefinitiASTParser.definitiContextToAST(result))
    }
  }

  def parseCoreDefinitionFile(source: String): Either[Seq[ErrorItem], Seq[ClassDefinition]] = {
    val in = CharStreams.fromFileName(source)
    val lexer = new CoreDefinitionLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = new CoreDefinitionParser(tokens)
    val errorListener = new ErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    val result: CoreDefinitionParser.CoreDefinitionContext = parser.coreDefinition()
    if (errorListener.hasError) {
      Left(errorListener.errors)
    } else {
      Right(CoreDefinitionASTParser.definitionContextToAST(result))
    }
  }
}
