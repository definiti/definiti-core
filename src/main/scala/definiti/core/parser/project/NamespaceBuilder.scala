package definiti.core.parser.project

import definiti.common.ast._
import definiti.common.utils.StringUtils
import definiti.core.Configuration

class NamespaceBuilder(fileContent: FileContent, configuration: Configuration) {
  private val imports: Map[String, String] = {
    val internalImports = fileContent.elements.collect {
      case aliasType: AliasType => aliasType.name -> fullName(aliasType.name)
      case definedType: DefinedType => definedType.name -> fullName(definedType.name)
      case enum: Enum => enum.name -> fullName(enum.name)
      case namedFunction: NamedFunction => namedFunction.name -> fullName(namedFunction.name)
      case verification: Verification => verification.name -> fullName(verification.name)
    }

    fileContent.imports ++ internalImports.toMap
  }

  def build(): Namespace = {
    Namespace(
      name = StringUtils.lastPart(fileContent.packageName),
      fullName = fileContent.packageName,
      elements = fileContent.elements.map(processNamespaceElement)
    )
  }

  private def processNamespaceElement(namespaceElement: NamespaceElement): NamespaceElement = {
    namespaceElement match {
      case aliasType: AliasType => processAliasType(aliasType)
      case definedType: DefinedType => processDefinedType(definedType)
      case enum: Enum => processEnum(enum)
      case namedFunction: NamedFunction => processNamedFunction(namedFunction)
      case verification: Verification => processVerification(verification)
      case extendedContext: ExtendedContext[_] => processExtendedContext(extendedContext)
      case nativeClassDefinition: NativeClassDefinition => nativeClassDefinition
    }
  }

  private def processAliasType(aliasType: AliasType): AliasType = {
    aliasType.copy(
      fullName = fullName(aliasType.name),
      parameters = aliasType.parameters.map(processParameterDefinition),
      alias = processTypeDeclaration(aliasType.alias),
      inherited = aliasType.inherited.map(processVerificationReference),
      verifications = aliasType.verifications.map(processTypeVerification)
    )
  }

  private def processParameterDefinition(parameterDefinition: ParameterDefinition): ParameterDefinition = {
    parameterDefinition.copy(
      typeReference = processAbstractTypeReference(parameterDefinition.typeReference)
    )
  }

  private def processVerificationReference(verificationReference: VerificationReference): VerificationReference = {
    verificationReference.copy(
      verificationName = normalizeImport(verificationReference.verificationName)
    )
  }

  private def processTypeVerification(typeVerification: TypeVerification): TypeVerification = {
    typeVerification match {
      case atomicTypeVerification: AtomicTypeVerification =>
        atomicTypeVerification.copy(
          message = processMessage(atomicTypeVerification.message),
          function = processFunction(atomicTypeVerification.function)
        )
      case dependentTypeVerification: DependentTypeVerification =>
        dependentTypeVerification.copy(
          message = processMessage(dependentTypeVerification.message),
          function = processFunction(dependentTypeVerification.function)
        )
    }
  }

  private def processFunction(definedFunction: DefinedFunction): DefinedFunction = {
    definedFunction.copy(
      parameters = definedFunction.parameters.map(processParameterDefinition),
      body = processExpression(definedFunction.body)
    )
  }

  private def processDefinedType(definedType: DefinedType): DefinedType = {
    definedType.copy(
      fullName = fullName(definedType.name),
      parameters = definedType.parameters.map(processParameterDefinition),
      attributes = definedType.attributes.map(processAttributeDefinition),
      verifications = definedType.verifications.map(processTypeVerification),
      inherited = definedType.inherited.map(processVerificationReference)
    )
  }

  private def processAttributeDefinition(attributeDefinition: AttributeDefinition): AttributeDefinition = {
    attributeDefinition.copy(
      typeDeclaration = processTypeDeclaration(attributeDefinition.typeDeclaration),
      verifications = attributeDefinition.verifications.map(processVerificationReference)
    )
  }

  private def processEnum(enum: Enum): Enum = {
    enum.copy(
      fullName = fullName(enum.name)
    )
  }

  private def processNamedFunction(namedFunction: NamedFunction): NamedFunction = {
    namedFunction.copy(
      fullName = fullName(namedFunction.name),
      parameters = namedFunction.parameters.map(processParameterDefinition),
      body = processExpression(namedFunction.body)
    )
  }

  private def processVerification(verification: Verification): Verification = {
    verification.copy(
      fullName = fullName(verification.name),
      parameters = verification.parameters.map(processParameterDefinition),
      message = processMessage(verification.message),
      function = processFunction(verification.function)
    )
  }

  private def processMessage(message: VerificationMessage): VerificationMessage = {
    message match {
      case literalMessage: LiteralMessage =>
        literalMessage
      case typedMessage: TypedMessage =>
        typedMessage.copy(
          types = typedMessage.types.map(processTypeReference)
        )
    }
  }

  private def processExpression(expression: Expression): Expression = {
    expression match {
      case logicalExpression: LogicalExpression =>
        logicalExpression.copy(
          left = processExpression(logicalExpression.left),
          right = processExpression(logicalExpression.right)
        )

      case calculatorExpression: CalculatorExpression =>
        calculatorExpression.copy(
          left = processExpression(calculatorExpression.left),
          right = processExpression(calculatorExpression.right)
        )

      case not: Not =>
        not.copy(
          inner = processExpression(not.inner)
        )

      case booleanValue: BooleanValue => booleanValue
      case integerValue: IntegerValue => integerValue
      case numberValue: NumberValue => numberValue
      case quotedStringValue: QuotedStringValue => quotedStringValue
      case reference: Reference =>
        reference.copy(
          name = normalizeImport(reference.name)
        )

      case methodCall: MethodCall =>
        methodCall.copy(
          expression = processExpression(methodCall.expression),
          parameters = methodCall.parameters.map(processExpression),
          generics = methodCall.generics.map(processTypeReference)
        )
      case attributeCall: AttributeCall =>
        attributeCall.copy(
          expression = processExpression(attributeCall.expression)
        )

      case combinedExpression: CombinedExpression =>
        combinedExpression.copy(
          parts = combinedExpression.parts.map(processExpression)
        )

      case condition: Condition =>
        condition.copy(
          condition = processExpression(condition.condition),
          onTrue = processExpression(condition.onTrue),
          onFalse = condition.onFalse.map(processExpression)
        )

      case lambdaExpression: LambdaExpression =>
        lambdaExpression.copy(
          parameterList = lambdaExpression.parameterList.map(processParameterDefinition),
          expression = processExpression(lambdaExpression.expression)
        )
      case functionCall: FunctionCall =>
        functionCall.copy(
          name = normalizeImport(functionCall.name),
          parameters = functionCall.parameters.map(processExpression),
          generics = functionCall.generics.map(processTypeReference)
        )

      case okValue: OkValue => okValue
      case koValue: KoValue =>
        koValue.copy(
          parameters = koValue.parameters.map(processExpression)
        )
    }
  }

  private def processExtendedContext(context: ExtendedContext[_]): ExtendedContext[_] = {
    configuration.contexts
      .find(_.contextName == context.name)
      .map { contextPlugin =>
        ExtendedContext(
          name = context.name,
          content = contextPlugin.parse(context.content.toString, fileContent.packageName, imports, context.innerLocation),
          innerLocation = context.innerLocation,
          location = context.location
        )
      }
      .getOrElse(context)
  }

  private def processTypeDeclaration(typeDeclaration: TypeDeclaration): TypeDeclaration = {
    typeDeclaration.copy(
      typeName = normalizeImport(typeDeclaration.typeName),
      genericTypes = typeDeclaration.genericTypes.map(processTypeDeclaration)
    )
  }

  private def fullName(value: String): String = {
    StringUtils.canonical(fileContent.packageName, value)
  }

  private def processAbstractTypeReference(abstractTypeReference: AbstractTypeReference): AbstractTypeReference = {
    abstractTypeReference match {
      case lambdaReference: LambdaReference =>
        lambdaReference.copy(
          inputTypes = lambdaReference.inputTypes.map(processTypeReference),
          outputType = processTypeReference(lambdaReference.outputType)
        )

      case namedFunctionReference: NamedFunctionReference =>
        namedFunctionReference.copy(
          normalizeImport(namedFunctionReference.functionName)
        )

      case typeReference: TypeReference =>
        processTypeReference(typeReference)

      case Unset => Unset
    }
  }

  private def processTypeReference(typeReference: TypeReference): TypeReference = {
    typeReference.copy(
      typeName = normalizeImport(typeReference.typeName),
      genericTypes = typeReference.genericTypes.map(processTypeReference)
    )
  }

  private def normalizeImport(name: String): String = {
    name.split("\\.").toList match {
      case Nil => ""
      case head :: tail => (imports.getOrElse(head, head) +: tail).mkString(".")
    }
  }
}

