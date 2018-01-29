package definiti.core.validation

import definiti.core.ProgramResult.NoResult
import definiti.core.ast._
import definiti.core._

private[core] class ASTValidation(
  val configuration: Configuration,
  val library: Library
) extends CommonValidation
  with ExpressionValidation
  with NamedFunctionValidation
  with TypeValidation
  with VerificationValidation {

  val controls = new Controls(configuration)

  def validate(root: Root): Program[NoResult] = {
    for {
      _ <- Program.validation(Validation.join(root.elements.map(validatePackageElement)))
      _ <- controls.validate(root, library)
    } yield NoResult
  }

  def validatePackage(thePackage: Namespace): Validation = {
    Validation.join(thePackage.elements.map(validatePackageElement))
  }

  def validatePackageElement(element: NamespaceElement): Validation = {
    element match {
      case subPackage: Namespace => validatePackage(subPackage)
      case verification: Verification => validateVerification(verification)
      case definedType: DefinedType => validateDefinedType(definedType)
      case aliasType: AliasType => validateAliasType(aliasType)
      case enum: Enum => validateEnum(enum)
      case namedFunction: NamedFunction => validateNamedFunction(namedFunction)
      case extendedContext: ExtendedContext[_] => validateExtendedContext(extendedContext, library)
    }
  }

  def validateExtendedContext[A](context: ExtendedContext[A], library: Library): Validation = {
    configuration.contexts
      .find(_.contextName == context.name)
      .map { contextPlugin =>
        // asInstanceOf because it should be the exact same plugin than for parsing.
        contextPlugin.asInstanceOf[ContextPlugin[A]]
          .validate(context.content, library)
      }
      .getOrElse(Valid)
  }
}
