package definiti.core.validation

import cats.implicits._
import definiti.common.ast.{Library, _}
import definiti.common.plugin.ContextPlugin
import definiti.common.program.Program
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation.{Valid, Validated}
import definiti.core._

private[core] class ASTValidation(configuration: Configuration, library: Library) {
  val controls = new Controls(configuration.programConfiguration)

  def validate(root: Root): Program[NoResult] = {
    for {
      _ <- controls.validate(root, library)
      _ <- Program.validated(validateExtendedContexts())
    } yield NoResult
  }

  private def validateExtendedContexts(): Validated[NoResult] = {
    Validated.squash {
      library.root.namespaces.flatMap(_.elements).collect {
        case extendedContext: ExtendedContext[_] => validateExtendedContext(extendedContext, library)
      }
    }.map(_ => NoResult)
  }

  def validateExtendedContext[A](context: ExtendedContext[A], library: Library): Validated[NoResult] = {
    configuration.contexts
      .find(_.contextName == context.name)
      .map { contextPlugin =>
        // asInstanceOf because it should be the exact same plugin than for parsing.
        contextPlugin.asInstanceOf[ContextPlugin[A]]
          .validate(context.content, library)
      }
      .getOrElse(Valid(NoResult))
  }
}
