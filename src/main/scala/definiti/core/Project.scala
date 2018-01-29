package definiti.core

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import definiti.core.ProgramResult.NoResult
import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.linking.ProjectLinking
import definiti.core.parser.api.CoreParser
import definiti.core.parser.project.ProjectParser
import definiti.core.structure.ProjectStructure
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
      parsedPureRoot <- projectParser.parse()
      core <- coreParser.parse()
      linkedPureRoot = ProjectLinking.injectLinks(parsedPureRoot, core)
      finalPureRoot <- processPluginParsers(linkedPureRoot)
      context = createProjectContext(finalPureRoot, core)
      typedRoot <- new ProjectTyping(context).addTypes(finalPureRoot)
      structuredRoot = new ProjectStructure(typedRoot).generateStructure()
      library = Library(structuredRoot, pureCoreToStructureCore(core))
      _ <-  processInternalValidation(structuredRoot, library)
      _ <- processExternalValidation(structuredRoot, library)
    } yield (structuredRoot, library)
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

  private def processPluginParsers(root: PureRoot): Program[PureRoot] = Program.validated {
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

  private def processInternalValidation(root: Root, library: Library): Program[NoResult] = Program.validation {
    new ASTValidation(configuration, library).validate(root)
  }

  private def processExternalValidation(root: Root, library: Library): Program[NoResult] = Program.validation {
    Validation.join(configuration.validators.map(_.validate(root, library)))
  }

  private def processGenerators(root: Root, library: Library): Map[Path, String] = {
    configuration.generators.flatMap(_.generate(root, library)).toMap
  }

  private def createProjectContext(root: PureRoot, core: Seq[PureClassDefinition]): ReferenceContext = {
    ReferenceContext(
      classes = core ++ root.files.flatMap(_.classDefinitions),
      verifications = root.files.flatMap(_.verifications),
      namedFunctions = root.files.flatMap(_.namedFunctions)
    )
  }
}
