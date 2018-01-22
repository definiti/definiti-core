package definiti.core

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.linking.ProjectLinking
import definiti.core.parser.{ProjectParser, ProjectParsingResult}
import definiti.core.structure.ProjectStructure
import definiti.core.typing.ProjectTyping
import definiti.core.validation._

import scala.collection.JavaConverters._

class Project(configuration: Configuration) {
  private val projectParser: ProjectParser = new ProjectParser(configuration)

  def process(): Validation = {
    generateStructureWithLibrary()
      .foreach { case (structuredRoot, library) =>
        processGenerators(structuredRoot, library)
          .foreach { case (path, content) =>
            Files.createDirectories(path.getParent)
            Files.write(path, Seq(content).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
          }
      }
      .toValidation
  }

  def generatePublicAST(): Validated[Root] = {
    generateStructureWithLibrary()
      .map(_._1)
  }

  def generateStructureWithLibrary(): Validated[(Root, Library)] = {
    processInternalParser()
      .flatMap { projectParsingResult =>
        processPluginParsers(projectParsingResult.root)
          .map { updatedRoot => projectParsingResult.copy(root = updatedRoot) }
      }
      .map { projectParsingResult =>
        (projectParsingResult, createProjectContext(projectParsingResult))
      }
      .flatMap { case (projectParsingResult, context) =>
        val typing = new ProjectTyping(context)
        val typedRoot = typing.addTypes(projectParsingResult.root)
        typedRoot.map((_, projectParsingResult.core))
      }
      .map { case (typedRoot, core) =>
        val projectStructure = new ProjectStructure(typedRoot)
        val structuredRoot = projectStructure.generateStructure()
        val library = Library(structuredRoot, pureCoreToStructureCore(core))
        (structuredRoot, library)
      }
      .filter { case (structuredRoot, library) =>
        processInternalValidation(structuredRoot, library)
          .and(processExternalValidation(structuredRoot, library))
      }
  }

  private def pureCoreToStructureCore(pureCore: Seq[PureClassDefinition]): Seq[ClassDefinition] = {
    pureCore.collect {
      case nativeClassDefinition: PureNativeClassDefinition =>
        NativeClassDefinition(
          name = nativeClassDefinition.name,
          fullName = nativeClassDefinition.name,
          genericTypes = nativeClassDefinition.genericTypes,
          attributes = nativeClassDefinition.attributes,
          methods = nativeClassDefinition.methods,
          comment = nativeClassDefinition.comment
        )
    }
  }

  private def processInternalParser(): Validated[ProjectParsingResult] = {
    projectParser.buildAST()
      .map(ProjectLinking.injectLinks)
  }

  private def processPluginParsers(root: PureRoot): Validated[PureRoot] = {
    // Do not accumulate errors because of eventual dependencies between plugins
    // See what is done and validate or update behavior
    val initialRoot: Validated[PureRoot] = ValidValue(root)
    configuration.parsers.foldLeft(initialRoot) { case (acc, parser) =>
      acc match {
        case errors@Invalid(_) => errors
        case ValidValue(updatedRoot) => parser.transform(updatedRoot)
      }
    }
  }

  private def processInternalValidation(root: Root, library: Library): Validation = {
    val astValidation = new ASTValidation(configuration, library)
    astValidation.validate(root)
  }

  private def processExternalValidation(root: Root, library: Library): Validation = {
    Validation.join(configuration.validators.map(_.validate(root, library)))
  }

  private def processGenerators(root: Root, library: Library): Map[Path, String] = {
    configuration.generators.flatMap(_.generate(root, library)).toMap
  }

  private def createProjectContext(projectParsingResult: ProjectParsingResult): ReferenceContext = {
    ReferenceContext(
      classes = projectParsingResult.core ++ projectParsingResult.root.files.flatMap(_.classDefinitions),
      verifications = projectParsingResult.root.files.flatMap(_.verifications),
      namedFunctions = projectParsingResult.root.files.flatMap(_.namedFunctions)
    )
  }
}
