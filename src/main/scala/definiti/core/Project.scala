package definiti.core

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import definiti.core.ast.pure.Root
import definiti.core.linking.ProjectLinking
import definiti.core.parser.{ProjectParser, ProjectParsingResult}
import definiti.core.validation._

import scala.collection.JavaConverters._

private[core] class Project(configuration: Configuration) {
  private val projectParser: ProjectParser = new ProjectParser(configuration)
  private val astValidation: ASTValidation = new ASTValidation(configuration)

  def process(): Validation = {
    processInternalParser()
      .flatMap { projectParsingResult =>
        processPluginParsers(projectParsingResult.root)
          .map { updatedRoot => projectParsingResult.copy(root = updatedRoot) }
      }
      .map { projectParsingResult =>
        (projectParsingResult, createProjectContext(projectParsingResult))
      }
      .filter { case (projectParsingResult, context) =>
        processInternalValidation(projectParsingResult.root, context)
          .and(processExternalValidation(projectParsingResult.root, context))
      }
      .foreach { case (projectParsingResult, context) =>
        processGenerators(projectParsingResult.root, context)
          .foreach { case (path, content) =>
            Files.createDirectories(path.getParent)
            Files.write(path, Seq(content).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
          }
      }
      .toValidation
  }

  private def processInternalParser(): Validated[ProjectParsingResult] = {
    projectParser.buildAST()
      .map(ProjectLinking.injectLinks)
  }

  private def processPluginParsers(root: Root): Validated[Root] = {
    // Do not accumulate errors because of eventual dependencies between plugins
    // See what is done and validate or update behavior
    val initialRoot: Validated[Root] = ValidValue(root)
    configuration.parsers.foldLeft(initialRoot) { case (acc, parser) =>
      acc match {
        case errors@Invalid(_) => errors
        case ValidValue(updatedRoot) => parser.transform(updatedRoot)
      }
    }
  }

  private def processInternalValidation(root: Root, context: ReferenceContext): Validation = {
    astValidation.validate(root)(context)
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
      namedFunctions = projectParsingResult.root.files.flatMap(_.namedFunctions)
    )
  }
}
