package definiti.core.validation

import definiti.core._
import definiti.core.ast.structure._

private[core] class ASTValidation(
  val configuration: Configuration,
  val library: Library
) extends CommonValidation
  with ExpressionValidation
  with NamedFunctionValidation
  with TypeValidation
  with VerificationValidation {

  def validate(root: Root): Validation = {
    Validation.join(root.elements.map(validatePackageElement))
  }

  def validatePackage(thePackage: Package): Validation = {
    Validation.join(thePackage.elements.map(validatePackageElement))
  }

  def validatePackageElement(element: PackageElement): Validation = {
    element match {
      case subPackage: Package => validatePackage(subPackage)
      case verification: Verification => validateVerification(verification)
      case definedType: DefinedType => validateDefinedType(definedType)
      case aliasType: AliasType => validateAliasType(aliasType)
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
