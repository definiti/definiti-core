package definiti.core.typing

import definiti.common.ast._
import definiti.common.program.Program
import definiti.common.validation.{Valid, Validated}
import definiti.core._

private[core] class ProjectTyping(context: Context) {
  def addTypes(root: Root): Program[Root] = Program.validated {
    Validated.squash(root.namespaces.map(addTypesIntoNamespace))
      .map(Root)
  }

  def addTypesIntoNamespace(namespace: Namespace): Validated[Namespace] = {
    Validated.squash {
      namespace.elements.map {
        case verification: Verification => addTypesIntoVerification(verification)
        case classDefinition: ClassDefinition => addTypeIntoClassDefinition(classDefinition)
        case namedFunction: NamedFunction => addTypesIntoNamedFunction(namedFunction)
        case other => Valid(other)
      }
    } map { elements =>
      namespace.copy(elements = elements)
    }
  }

  def addTypesIntoVerification(verification: Verification): Validated[Verification] = {
    val verificationContext = VerificationContext(context, verification)
    val definedFunctionContext = DefinedFunctionContext(verificationContext, verification.function)
    val validatedFunction = new FunctionTyping(definedFunctionContext).addTypesIntoDefinedFunction(verification.function)
    validatedFunction.map { function =>
      verification.copy(function = function)
    }
  }

  def addTypeIntoClassDefinition(classDefinition: ClassDefinition): Validated[ClassDefinition] = {
    val classDefinitionContext = ClassContext(context, classDefinition)
    new ClassDefinitionTyping(classDefinitionContext).addTypesIntoClassDefinition(classDefinition)
  }

  def addTypesIntoNamedFunction(namedFunction: NamedFunction): Validated[NamedFunction] = {
    val namedFunctionContext = NamedFunctionReferenceContext(
      outerContext = context,
      currentFunction = namedFunction
    )
    val validatedExpression = new ExpressionTyping(namedFunctionContext).addTypesIntoExpression(namedFunction.body)
    validatedExpression.map { expression =>
      namedFunction.copy(body = expression)
    }
  }
}
