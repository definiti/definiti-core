package definiti.core.typing

import definiti.core._
import definiti.core.ast.pure.{PureNamedFunction, PureRoot, PureRootFile, PureVerification}
import definiti.core.ast.typed.{TypedNamedFunction, TypedRoot, TypedRootFile, TypedVerification}

private[core] class ProjectTyping(context: Context) {
  val classDefinitionTyping = new ClassDefinitionTyping(context)
  val functionTyping = new FunctionTyping(context)
  val expressionTyping = new ExpressionTyping(context)

  def addTypes(root: PureRoot): Validated[TypedRoot] = {
    Validated.squash(root.files.map(addTypesIntoRootFile))
      .map(files => TypedRoot(files))
  }

  def addTypesIntoRootFile(rootFile: PureRootFile): Validated[TypedRootFile] = {
    val validatedTypedVerifications = Validated.squash(rootFile.verifications.map(addTypesIntoVerification))
    val validatedTypedClassDefinition = Validated.squash(rootFile.classDefinitions.map(classDefinitionTyping.addTypesIntoClassDefinition))
    val validatedTypedNamedFunction = Validated.squash(rootFile.namedFunctions.map(addTypesIntoNamedFunction))
    Validated.both(validatedTypedVerifications, validatedTypedClassDefinition, validatedTypedNamedFunction)
      .map { case (verifications, classDefinitions, namedFunctions) =>
        TypedRootFile(
          packageName = rootFile.packageName,
          verifications = verifications,
          classDefinitions = classDefinitions,
          namedFunctions = namedFunctions,
          contexts = rootFile.contexts
        )
      }
  }

  def addTypesIntoVerification(verification: PureVerification): Validated[TypedVerification] = {
    val validatedFunction = functionTyping.addTypesIntoDefinedFunction(verification.function)
    validatedFunction.map { function =>
      TypedVerification(
        name = verification.name,
        packageName = verification.packageName,
        message = verification.message,
        function = function,
        comment = verification.comment,
        range = verification.range
      )
    }
  }

  def addTypesIntoNamedFunction(namedFunction: PureNamedFunction): Validated[TypedNamedFunction] = {
    val validatedExpression = expressionTyping.addTypesIntoExpression(namedFunction.body)
    validatedExpression.map { expression =>
      TypedNamedFunction(
        name = namedFunction.name,
        packageName = namedFunction.packageName,
        genericTypes = namedFunction.genericTypes,
        parameters = namedFunction.parameters,
        returnType = namedFunction.returnType,
        body = expression,
        range = namedFunction.range
      )
    }
  }
}
