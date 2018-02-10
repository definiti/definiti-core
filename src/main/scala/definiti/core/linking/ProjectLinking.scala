package definiti.core.linking

import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.utils.StringUtils

private[core] object ProjectLinking {
  def injectLinks(root: PureRoot, core: Seq[PureClassDefinition]): PureRoot = {
    val coreTypeMapping = extractTypeMappingFromCore(core)
    injectLinksIntoRoot(root, coreTypeMapping)
  }

  def extractTypeMappingFromCore(core: Seq[PureClassDefinition]): TypeMapping = {
    core
      .view
      .map(_.name)
      .map(name => StringUtils.lastPart(name) -> name)
      .toMap
  }

  def injectLinksIntoRoot(root: PureRoot, coreTypeMapping: TypeMapping): PureRoot = {
    root.copy(
      root.files.map(injectLinksIntoRootFile(_, coreTypeMapping))
    )
  }

  def injectLinksIntoRootFile(rootFile: PureRootFile, coreTypeMapping: TypeMapping): PureRootFile = {
    val fileTypeMapping = extractTypeMappingFromFile(rootFile)
    val imports = rootFile.imports
    val typeMapping = coreTypeMapping ++ fileTypeMapping ++ imports
    rootFile.copy(
      verifications = rootFile.verifications.map(injectLinksIntoVerification(_, rootFile.packageName, typeMapping)),
      classDefinitions = rootFile.classDefinitions.map(injectLinksIntoClassDefinition(_, rootFile.packageName, typeMapping)),
      namedFunctions = rootFile.namedFunctions.map(injectLinksIntoNamedFunction(_, rootFile.packageName, typeMapping))
    )
  }

  def extractTypeMappingFromFile(rootFile: PureRootFile): TypeMapping = {
    val packageNamePrefix = if (rootFile.packageName.nonEmpty) {
      rootFile.packageName + "."
    } else {
      ""
    }
    val verificationTypeMapping = rootFile.verifications
      .view
      .map(_.name)
      .map(name => name -> (packageNamePrefix + name))
      .toMap
    val classDefinitionTypeMapping = rootFile.classDefinitions
      .view
      .map(_.name)
      .map(name => name -> (packageNamePrefix + name))
      .toMap
    val namedFunctionTypeMapping = rootFile.namedFunctions
      .view
      .map(_.name)
      .map(name => name -> (packageNamePrefix + name))
      .toMap

    verificationTypeMapping ++ classDefinitionTypeMapping ++ namedFunctionTypeMapping
  }

  def injectLinksIntoVerification(verification: PureVerification, packageName: String, typeMapping: TypeMapping): PureVerification = {
    verification.copy(
      packageName = packageName,
      parameters = verification.parameters.map(injectLinksIntoParameter(_, typeMapping)),
      message = injectLinksIntoVerificationMessage(verification.message, typeMapping),
      function = injectLinksIntoFunction(verification.function, typeMapping)
    )
  }

  def injectLinksIntoVerificationMessage(verificationMessage: VerificationMessage, typeMapping: TypeMapping): VerificationMessage = {
    verificationMessage match {
      case message: LiteralMessage => message
      case typedMessage: TypedMessage => typedMessage.copy(
        types = typedMessage.types.map(injectLinksIntoTypeReference(_, typeMapping))
      )
    }
  }

  def injectLinksIntoClassDefinition(classDefinition: PureClassDefinition, packageName: String, typeMapping: TypeMapping): PureClassDefinition = {
    classDefinition match {
      case aliasType: PureAliasType =>
        aliasType.copy(
          packageName = packageName,
          alias = injectLinksIntoTypeReference(aliasType.alias, typeMapping),
          verifications = aliasType.verifications.map(injectLinksIntoTypeVerification(_, typeMapping)),
          inherited = aliasType.inherited.map(injectLinksIntoVerificationReference(_, typeMapping))
        )
      case definedType: PureDefinedType =>
        definedType.copy(
          packageName = packageName,
          attributes = definedType.attributes.map(injectLinksIntoAttributes(_, typeMapping)),
          verifications = definedType.verifications.map(injectLinksIntoTypeVerification(_, typeMapping)),
          inherited = definedType.inherited.map(injectLinksIntoVerificationReference(_, typeMapping))
        )
      case enum: PureEnum =>
        enum.copy(packageName = packageName)
      case other => other
    }
  }

  def injectLinksIntoAttributes(attributeDefinition: AttributeDefinition, typeMapping: TypeMapping): AttributeDefinition = {
    attributeDefinition.copy(
      typeReference = injectLinksIntoTypeReference(attributeDefinition.typeReference, typeMapping),
      verifications = attributeDefinition.verifications.map(injectLinksIntoVerificationReference(_, typeMapping))
    )
  }

  def injectLinksIntoTypeVerification(typeVerification: PureTypeVerification, typeMapping: TypeMapping): PureTypeVerification = {
    typeVerification.copy(
      message = injectLinksIntoVerificationMessage(typeVerification.message, typeMapping),
      function = injectLinksIntoFunction(typeVerification.function, typeMapping)
    )
  }

  def injectLinksIntoNamedFunction(namedFunction: PureNamedFunction, packageName: String, typeMapping: TypeMapping): PureNamedFunction = {
    namedFunction.copy(
      packageName = packageName,
      parameters = namedFunction.parameters.map(injectLinksIntoParameter(_, typeMapping)),
      returnType = injectLinksIntoTypeReference(namedFunction.returnType, typeMapping),
      body = injectLinksIntoExpression(namedFunction.body, typeMapping)
    )
  }

  def injectLinksIntoFunction(function: PureDefinedFunction, typeMapping: TypeMapping): PureDefinedFunction = {
    function.copy(
      parameters = function.parameters.map(injectLinksIntoParameter(_, typeMapping)),
      body = injectLinksIntoExpression(function.body, typeMapping)
    )
  }

  def injectLinksIntoParameter(parameterDefinition: ParameterDefinition, typeMapping: TypeMapping): ParameterDefinition = {
    parameterDefinition.copy(
      typeReference = injectLinksIntoAbstractTypeReference(parameterDefinition.typeReference, typeMapping)
    )
  }

  def injectLinksIntoAbstractTypeReference(abstractTypeReference: AbstractTypeReference, typeMapping: TypeMapping): AbstractTypeReference = {
    abstractTypeReference match {
      case typeReference: TypeReference => injectLinksIntoTypeReference(typeReference, typeMapping)
      case lambdaReference: LambdaReference => injectLinksIntoLambdaReference(lambdaReference, typeMapping)
    }
  }

  def injectLinksIntoTypeReference(typeReference: TypeReference, typeMapping: TypeMapping): TypeReference = {
    typeReference.copy(
      typeName = getLink(typeReference.typeName, typeMapping),
      genericTypes = typeReference.genericTypes.map(injectLinksIntoTypeReference(_, typeMapping))
    )
  }

  def injectLinksIntoLambdaReference(lambdaReference: LambdaReference, typeMapping: TypeMapping): LambdaReference = {
    lambdaReference.copy(
      inputTypes = lambdaReference.inputTypes.map(injectLinksIntoTypeReference(_, typeMapping)),
      outputType = injectLinksIntoTypeReference(lambdaReference.outputType, typeMapping)
    )
  }

  def injectLinksIntoVerificationReference(verificationReference: VerificationReference, typeMapping: TypeMapping): VerificationReference = {
    verificationReference.copy(
      verificationName = getLink(verificationReference.verificationName, typeMapping)
    )
  }

  def injectLinksIntoExpression(expression: PureExpression, typeMapping: TypeMapping): PureExpression = {
    expression match {
      case logicalExpression: PureLogicalExpression =>
        logicalExpression.copy(
          left = injectLinksIntoExpression(logicalExpression.left, typeMapping),
          right = injectLinksIntoExpression(logicalExpression.right, typeMapping)
        )
      case calculatorExpression: PureCalculatorExpression =>
        calculatorExpression.copy(
          left = injectLinksIntoExpression(calculatorExpression.left, typeMapping),
          right = injectLinksIntoExpression(calculatorExpression.right, typeMapping)
        )
      case not: PureNot =>
        not.copy(inner = injectLinksIntoExpression(not.inner, typeMapping))
      case booleanValue: PureBooleanValue => booleanValue
      case numberValue: PureNumberValue => numberValue
      case quotedStringValue: PureQuotedStringValue => quotedStringValue
      case reference: PureReference =>
        reference.copy(
          name = getLink(reference.name, typeMapping)
        )
      case methodCall: PureMethodCall =>
        methodCall.copy(
          expression = injectLinksIntoExpression(methodCall.expression, typeMapping),
          parameters = methodCall.parameters.map(injectLinksIntoExpression(_, typeMapping)),
          generics = methodCall.generics.map(injectLinksIntoTypeReference(_, typeMapping))
        )
      case attributeCall: PureAttributeCall =>
        attributeCall.copy(
          expression = injectLinksIntoExpression(attributeCall.expression, typeMapping)
        )
      case combinedExpression: PureCombinedExpression =>
        combinedExpression.copy(
          parts = combinedExpression.parts.map(injectLinksIntoExpression(_, typeMapping))
        )
      case condition: PureCondition =>
        condition.copy(
          condition = injectLinksIntoExpression(condition.condition, typeMapping),
          onTrue = injectLinksIntoExpression(condition.onTrue, typeMapping),
          onFalse = condition.onFalse.map(injectLinksIntoExpression(_, typeMapping))
        )
      case lambdaExpression: PureLambdaExpression =>
        lambdaExpression.copy(
          expression = injectLinksIntoExpression(lambdaExpression.expression, typeMapping),
          parameterList = lambdaExpression.parameterList.map(injectLinksIntoParameter(_, typeMapping))
        )
      case functionCallExpression: PureFunctionCall =>
        functionCallExpression.copy(
          name = getLink(functionCallExpression.name, typeMapping),
          parameters = functionCallExpression.parameters.map(injectLinksIntoExpression(_, typeMapping)),
          generics = functionCallExpression.generics.map(injectLinksIntoTypeReference(_, typeMapping))
        )
      case pureOkValue: PureOkValue => pureOkValue
      case pureKoValue: PureKoValue =>
        pureKoValue.copy(
          parameters = pureKoValue.parameters.map(injectLinksIntoExpression(_, typeMapping))
        )
    }
  }

  def getLink(name: String, typeMapping: TypeMapping): String = {
    typeMapping.getOrElse(name, name)
  }
}
