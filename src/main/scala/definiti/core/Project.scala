package definiti.core

import definiti.core.parser._

class Project(configuration: Configuration) {
  private val projectParser: ProjectParser = new ProjectParser(configuration)

  def buildAST(): Either[Seq[String], ProjectParsingResult] = {
    projectParser.buildAST() match {
      case Left(errors) => Left(errors.map(_.prettyPrint))
      case Right(projectParsingResult) => Right(projectParsingResult)
    }
  }

  def validateAST(root: Root, core: Seq[ClassDefinition]): Validation = {
    implicit val context = ReferenceContext(
      classes = core ++ root.files.flatMap(_.classDefinitions),
      verifications = root.files.flatMap(_.verifications)
    )
    ASTValidation.validate(root)
  }

  def load(): Either[Seq[String], Root] = {
    projectParser.buildAST() match {
      case Left(errors) =>
        Left(errors.map(_.prettyPrint))
      case Right(project) =>
        val projectWithLinks = ProjectLinking.injectLinks(project)
        validateAST(projectWithLinks.root, projectWithLinks.core) match {
          case Valid =>
            Right(projectWithLinks.root)
          case Invalid(errors) =>
            Left(errors.map(_.prettyPrint))
        }
    }
  }
}
