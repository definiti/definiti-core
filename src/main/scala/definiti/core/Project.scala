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
      classes = core ++ root.classDefinitions,
      verifications = root.verifications
    )
    ASTValidation.validate(root)
  }

  def load(): Either[Seq[String], Root] = {
    projectParser.buildAST() match {
      case Left(errors) =>
        Left(errors.map(_.prettyPrint))
      case Right(project) =>
        validateAST(project.root, project.core) match {
          case Valid =>
            Right(project.root)
          case Invalid(errors) =>
            Left(errors.map(_.prettyPrint))
        }
    }
  }
}
