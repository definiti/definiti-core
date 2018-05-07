package definiti.core.typing

import definiti.common.ast._
import definiti.common.program.Program
import definiti.common.utils.StringUtils
import definiti.common.validation.Validated
import definiti.core._
import definiti.core.ast.pure
import definiti.core.ast.pure._

private[core] class ProjectTyping(context: Context) {
  def addTypes(root: PureRoot): Program[Root] = Program.validated {
    Validated.squash(root.files.map(addTypesIntoRootFile))
      .map(mergeNamespaces)
      .map(Root)
  }

  def addTypesIntoRootFile(rootFile: PureRootFile): Validated[Namespace] = {
    val validatedTypedVerifications = Validated.squash(rootFile.verifications.map(addTypesIntoVerification))
    val validatedTypedClassDefinition = Validated.squash(rootFile.classDefinitions.map(addTypeIntoClassDefinition))
    val validatedTypedNamedFunction = Validated.squash(rootFile.namedFunctions.map(addTypesIntoNamedFunction))
    val extendedContexts = rootFile.contexts.map(transformExtendedContext(_))
    Validated.both(validatedTypedVerifications, validatedTypedClassDefinition, validatedTypedNamedFunction)
      .map { case (verifications, classDefinitions, namedFunctions) =>
        Namespace(
          name = StringUtils.lastPart(rootFile.packageName),
          fullName = rootFile.packageName,
          elements = verifications ++ classDefinitions ++ namedFunctions ++ extendedContexts
        )
      }
  }

  def addTypesIntoVerification(verification: PureVerification): Validated[Verification] = {
    val verificationContext = VerificationContext(context, verification)
    val definedFunctionContext = DefinedFunctionContext(verificationContext, verification.function)
    val validatedFunction = new FunctionTyping(definedFunctionContext).addTypesIntoDefinedFunction(verification.function)
    validatedFunction.map { function =>
      Verification(
        name = verification.name,
        fullName = StringUtils.canonical(verification.packageName, verification.name),
        parameters = verification.parameters,
        message = verification.message,
        function = function,
        comment = verification.comment,
        location = verification.location
      )
    }
  }

  def addTypeIntoClassDefinition(classDefinition: PureClassDefinition): Validated[ClassDefinition] = {
    val classDefinitionContext = ClassContext(context, classDefinition)
    new ClassDefinitionTyping(classDefinitionContext).addTypesIntoClassDefinition(classDefinition)
  }

  def addTypesIntoNamedFunction(namedFunction: PureNamedFunction): Validated[NamedFunction] = {
    val namedFunctionContext = NamedFunctionReferenceContext(
      outerContext = context,
      currentFunction = namedFunction
    )
    val validatedExpression = new ExpressionTyping(namedFunctionContext).addTypesIntoExpression(namedFunction.body)
    validatedExpression.map { expression =>
      NamedFunction(
        name = namedFunction.name,
        fullName = StringUtils.canonical(namedFunction.packageName, namedFunction.name),
        genericTypes = namedFunction.genericTypes,
        parameters = namedFunction.parameters,
        returnType = namedFunction.returnType,
        body = expression,
        location = namedFunction.location
      )
    }
  }

  private def transformExtendedContext[A](extendedContext: pure.PureExtendedContext[A]): ExtendedContext[A] = {
    ExtendedContext(
      name = extendedContext.name,
      content = extendedContext.content,
      location = extendedContext.location
    )
  }

  private def mergeNamespaces(namespaces: Seq[Namespace]): Seq[Namespace] = {
    namespaces.foldLeft(Map.empty[String, Namespace]) { (acc, current) =>
      acc.get(current.fullName) match {
        case Some(existingNamespace) =>
          acc + (existingNamespace.fullName -> existingNamespace.copy(
            elements = existingNamespace.elements ++ current.elements
          ))
        case None =>
          acc + (current.fullName -> current)
      }
    }.values.toSeq
  }
}
