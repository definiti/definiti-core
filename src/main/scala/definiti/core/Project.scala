package definiti.core

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import cats.implicits._
import definiti.common.ast._
import definiti.common.program.Program
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation.{Invalid, Valid, Validated}
import definiti.core.parser.api.CoreParser
import definiti.core.parser.project.ProjectParser
import definiti.core.typing.ProjectTyping
import definiti.core.validation._

import scala.collection.JavaConverters._

class Project(configuration: Configuration) {
  private val coreParser: CoreParser = new CoreParser(configuration)
  private val projectParser: ProjectParser = new ProjectParser(configuration)

  def program(): Program[NoResult] = {
    for {
      (root, library) <- generateStructureWithLibrary()
      files = processGenerators(root, library)
      _ = files.foreach { case (path, content) =>
        Files.createDirectories(path.getParent)
        Files.write(path, Seq(content).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      }
    } yield NoResult
  }

  def generatePublicAST(): Program[Root] = {
    for {
      (root, _) <- generateStructureWithLibrary()
    } yield root
  }

  def generateStructureWithLibrary(): Program[(Root, Library)] = {
    for {
      untypedRoot <- projectParser.parse()
      core <- coreParser.parse()
      finalUntypedRoot <- processPluginParsers(untypedRoot)
      context = createProjectContext(finalUntypedRoot, core)
      root <- new ProjectTyping(context).addTypes(finalUntypedRoot)
      library = Library(root, core)
      _ <- processInternalValidation(root, library)
      _ <- processExternalValidation(root, library)
    } yield (root, library)
  }

  private def processPluginParsers(root: Root): Program[Root] = Program.validated {
    // Do not accumulate errors because of eventual dependencies between plugins
    // See what is done and validate or update behavior
    val initialRoot: Validated[Root] = Valid(root)
    configuration.parsers.foldLeft(initialRoot) { case (acc, parser) =>
      acc match {
        case errors@Invalid(_) => errors
        case Valid(updatedRoot) => parser.transform(updatedRoot)
      }
    }
  }

  private def processInternalValidation(root: Root, library: Library): Program[NoResult] = {
    new ASTValidation(configuration, library).validate(root)
  }

  private def processExternalValidation(root: Root, library: Library): Program[NoResult] = Program.validated {
    Validated.squash(configuration.validators.map(_.validate(root, library))).map(_ => NoResult)
  }

  private def processGenerators(root: Root, library: Library): Map[Path, String] = {
    configuration.generators.flatMap(_.generate(root, library)).toMap
  }

  private def createProjectContext(root: Root, core: Seq[ClassDefinition]): ReferenceContext = {
    ReferenceContext(
      classes = core ++ root.namespaces.flatMap(_.elements).collect { case classDefinition: ClassDefinition => classDefinition },
      namedFunctions = root.namespaces.flatMap(_.elements).collect { case namedFunction: NamedFunction => namedFunction }
    )
  }
}
