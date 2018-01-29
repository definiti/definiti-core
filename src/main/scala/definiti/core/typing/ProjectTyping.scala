package definiti.core.typing

import definiti.core._
import definiti.core.ast.pure._
import definiti.core.ast.typed._

private[core] class ProjectTyping(context: Context) {
  def addTypes(root: PureRoot): Program[TypedRoot] = Program.validated {
    Validated.squash(root.files.map(addTypesIntoRootFile))
      .map(files => TypedRoot(files))
  }

  def addTypesIntoRootFile(rootFile: PureRootFile): Validated[TypedRootFile] = {
    val validatedTypedVerifications = Validated.squash(rootFile.verifications.map(addTypesIntoVerification))
    val validatedTypedClassDefinition = Validated.squash(rootFile.classDefinitions.map(addTypeIntoClassDefinition))
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
    val definedFunctionContext = DefinedFunctionContext(context, verification.function)
    val validatedFunction = new FunctionTyping(definedFunctionContext).addTypesIntoDefinedFunction(verification.function)
    validatedFunction.map { function =>
      TypedVerification(
        name = verification.name,
        packageName = verification.packageName,
        message = verification.message,
        function = function,
        comment = verification.comment,
        location = verification.location
      )
    }
  }

  def addTypeIntoClassDefinition(classDefinition: PureClassDefinition): Validated[TypedClassDefinition] = {
    val classDefinitionContext = ClassContext(context, classDefinition)
    new ClassDefinitionTyping(classDefinitionContext).addTypesIntoClassDefinition(classDefinition)
  }

  def addTypesIntoNamedFunction(namedFunction: PureNamedFunction): Validated[TypedNamedFunction] = {
    val namedFunctionContext = NamedFunctionReferenceContext(
      outerContext = context,
      currentFunction = namedFunction
    )
    val validatedExpression = new ExpressionTyping(namedFunctionContext).addTypesIntoExpression(namedFunction.body)
    validatedExpression.map { expression =>
      TypedNamedFunction(
        name = namedFunction.name,
        packageName = namedFunction.packageName,
        genericTypes = namedFunction.genericTypes,
        parameters = namedFunction.parameters,
        returnType = namedFunction.returnType,
        body = expression,
        location = namedFunction.location
      )
    }
  }
}
