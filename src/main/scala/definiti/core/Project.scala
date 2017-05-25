package definiti.core

import definiti.core.linking.ProjectLinking
import definiti.core.parser._
import definiti.core.validation.{ASTValidation, Invalid, Valid}

case class ProjectResult(
  root: Root,
  context: Context
)

class Project(configuration: Configuration) {
  private val projectParser: ProjectParser = new ProjectParser(configuration)

  def buildAST(): Either[Seq[String], ProjectParsingResult] = {
    projectParser.buildAST() match {
      case Left(errors) => Left(errors.map(_.prettyPrint))
      case Right(projectParsingResult) => Right(projectParsingResult)
    }
  }

  def load(): Either[Seq[String], ProjectResult] = {
    projectParser.buildAST() match {
      case Left(errors) =>
        Left(errors.map(_.prettyPrint))
      case Right(project) =>
        val projectWithLinks = ProjectLinking.injectLinks(project)
        val context = createProjectContext(projectWithLinks)
        ASTValidation.validate(projectWithLinks.root)(context) match {
          case Valid =>
            Right(ProjectResult(projectWithLinks.root, context))
          case Invalid(errors) =>
            Left(errors.map(_.prettyPrint))
        }
    }
  }

  private def createProjectContext(projectParsingResult: ProjectParsingResult) = {
    ReferenceContext(
      classes = projectParsingResult.core ++ projectParsingResult.root.files.flatMap(_.classDefinitions),
      verifications = projectParsingResult.root.files.flatMap(_.verifications),
      namedFunctions = projectParsingResult.root.files.flatMap(_.namedFunctions)
    )
  }
}
