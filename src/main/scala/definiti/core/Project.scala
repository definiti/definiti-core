package definiti.core

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import definiti.core.linking.ProjectLinking
import definiti.core.parser.{ProjectParser, ProjectParsingResult}
import definiti.core.validation._

import scala.collection.JavaConverters._

private[core] class Project(configuration: Configuration) {
  private val projectParser: ProjectParser = new ProjectParser(configuration.source, configuration.apiSource)

  def process(): Validation = {
    processInternalParser() match {
      case Left(errors) => Invalid(errors.map(SimpleError))
      case Right(projectParsingResult) =>
        processPluginParsers(projectParsingResult.root) match {
          case Left(errors) => Invalid(errors.map(SimpleError))
          case Right(updatedRoot) =>
            val context = createProjectContext(projectParsingResult.copy(root = updatedRoot))
            processInternalValidation(updatedRoot, context) match {
              case Invalid(errors) => Invalid(errors)
              case Valid =>
                processExternalValidation(updatedRoot, context) match {
                  case Invalid(errors) => Invalid(errors)
                  case Valid =>
                    processGenerators(updatedRoot, context)
                      .foreach { case (path, content) =>
                        Files.createDirectories(path.getParent)
                        Files.write(path, Seq(content).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
                      }
                    Valid
                }
            }
        }
    }
  }

  private def processInternalParser(): Either[Seq[String], ProjectParsingResult] = {
    projectParser.buildAST() match {
      case Left(errors) =>
        Left(errors.map(_.prettyPrint))
      case Right(projectParsingResult) =>
        Right(ProjectLinking.injectLinks(projectParsingResult))
    }
  }

  private def processPluginParsers(root: Root): Either[Seq[String], Root] = {
    // Do not accumulate errors because of eventual dependencies between plugins
    // See what is done and validate or update behavior
    val initialRoot: Either[Seq[String], Root] = Right(root)
    configuration.parsers.foldLeft(initialRoot) { case (acc, parser) =>
      acc match {
        case errors@Left(_) => errors
        case Right(updatedRoot) => parser.transform(updatedRoot)
      }
    }
  }

  private def processInternalValidation(root: Root, context: ReferenceContext): Validation = {
    ASTValidation.validate(root)(context)
  }

  private def processExternalValidation(root: Root, context: ReferenceContext): Validation = {
    Validation.join(configuration.validators.map(_.validate(root, context)))
  }

  private def processGenerators(root: Root, context: ReferenceContext): Map[Path, String] = {
    configuration.generators.flatMap(_.generate(root, context)).toMap
  }

  private def createProjectContext(projectParsingResult: ProjectParsingResult) = {
    ReferenceContext(
      classes = projectParsingResult.core ++ projectParsingResult.root.files.flatMap(_.classDefinitions),
      verifications = projectParsingResult.root.files.flatMap(_.verifications),
      namedFunctions = projectParsingResult.root.files.flatMap(_.namedFunctions),
      requirements = projectParsingResult.root.files.flatMap(_.http.flatMap(_.requirements))
    )
  }
}
