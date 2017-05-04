package definiti.core.parser

import definiti.core._
import definiti.core.utils.StringUtils

private[core] object ProjectLinking {
  type TypeMapping = Map[String, String]
  def emptyTypeMapping = Map.empty[String, String]

  def injectLinks(projectParsingResult: ProjectParsingResult): ProjectParsingResult = {
    val coreTypeMapping = extractTypeMappingFromCore(projectParsingResult.core)
    projectParsingResult.copy(
      root = injectLinksIntoRoot(projectParsingResult.root, coreTypeMapping)
    )
  }

  private def extractTypeMappingFromCore(core: Seq[ClassDefinition]): TypeMapping = {
    core
      .view
      .map(_.name)
      .map(name => StringUtils.lastPart(name, '.') -> name)
      .toMap
  }

  private def injectLinksIntoRoot(root: Root, coreTypeMapping: TypeMapping): Root = {
    root.copy(
      root.files.map(injectLinksIntoRootFile(_, coreTypeMapping))
    )
  }

  private def injectLinksIntoRootFile(rootFile: RootFile, coreTypeMapping: TypeMapping): RootFile = {
    val fileTypeMapping = extractTypeMappingFromFile(rootFile)
    val imports = rootFile.imports
    val typeMapping = coreTypeMapping ++ fileTypeMapping ++ imports
    rootFile.copy(
      verifications = rootFile.verifications.map(injectLinksIntoVerification(_, rootFile.packageName, typeMapping)),
      classDefinitions = rootFile.classDefinitions.map(injectLinksIntoClassDefinition(_, rootFile.packageName, typeMapping))
    )
  }

  private def extractTypeMappingFromFile(rootFile: RootFile): TypeMapping = {
    val verificationTypeMapping = rootFile.verifications
      .view
      .map(_.name)
      .map(name => name -> (rootFile.packageName + "." + name))
      .toMap
    val classDefinitionTypeMapping = rootFile.classDefinitions
      .view
      .map(_.name)
      .map(name => name -> (rootFile.packageName + "." + name))
      .toMap

    verificationTypeMapping ++ classDefinitionTypeMapping
  }

  private def injectLinksIntoVerification(verification: Verification, packageName: String, typeMapping: TypeMapping): Verification = {
    verification.copy(
      packageName = packageName,
      function = injectLinksIntoFunction(verification.function, typeMapping)
    )
  }

  private def injectLinksIntoClassDefinition(classDefinition: ClassDefinition, packageName: String, typeMapping: TypeMapping): ClassDefinition = {
    classDefinition match {
      case aliasType: AliasType =>
        aliasType.copy(
          packageName = packageName,
          alias = injectLinksIntoTypeReference(aliasType.alias, typeMapping),
          inherited = aliasType.inherited.map(getLink(_, typeMapping))
        )
      case definedType: DefinedType =>
        definedType.copy(
          packageName = packageName,
          attributes = definedType.attributes.map(injectLinksIntoAttributes(_, typeMapping)),
          verifications = definedType.verifications.map(injectLinksIntoTypeVerification(_, typeMapping)),
          inherited = definedType.inherited.map(getLink(_, typeMapping))
        )
      case other => other
    }
  }

  private def injectLinksIntoAttributes(attributeDefinition: AttributeDefinition, typeMapping: TypeMapping): AttributeDefinition = {
    attributeDefinition.copy(
      typeReference = injectLinksIntoTypeReference(attributeDefinition.typeReference, typeMapping)
    )
  }

  private def injectLinksIntoTypeVerification(typeVerification: TypeVerification, typeMapping: TypeMapping): TypeVerification = {
    typeVerification.copy(
      function = injectLinksIntoFunction(typeVerification.function, typeMapping)
    )
  }

  private def injectLinksIntoFunction(function: DefinedFunction, typeMapping: TypeMapping): DefinedFunction = {
    function.copy(
      parameters = function.parameters.map(injectLinksIntoParameter(_, typeMapping)),
      body = injectLinksIntoExpression(function.body, typeMapping)
    )
  }

  private def injectLinksIntoParameter(parameterDefinition: ParameterDefinition, typeMapping: TypeMapping): ParameterDefinition = {
    parameterDefinition.copy(
      typeReference = injectLinksIntoAbstractTypeReference(parameterDefinition.typeReference, typeMapping)
    )
  }

  private def injectLinksIntoAbstractTypeReference(abstractTypeReference: AbstractTypeReference, typeMapping: TypeMapping): AbstractTypeReference = {
    abstractTypeReference match {
      case typeReference: TypeReference => injectLinksIntoTypeReference(typeReference, typeMapping)
      case lambdaReference: LambdaReference => injectLinksIntoLambdaReference(lambdaReference, typeMapping)
    }
  }

  private def injectLinksIntoTypeReference(typeReference: TypeReference, typeMapping: TypeMapping): TypeReference = {
    typeReference.copy(
      typeName = getLink(typeReference.typeName, typeMapping),
      genericTypes = typeReference.genericTypes.map(injectLinksIntoTypeReference(_, typeMapping))
    )
  }

  private def injectLinksIntoLambdaReference(lambdaReference: LambdaReference, typeMapping: TypeMapping): LambdaReference = {
    lambdaReference.copy(
      inputTypes = lambdaReference.inputTypes.map(injectLinksIntoTypeReference(_, typeMapping)),
      outputType = injectLinksIntoTypeReference(lambdaReference.outputType, typeMapping)
    )
  }

  private def injectLinksIntoExpression(expression: Expression, typeMapping: TypeMapping): Expression = {
    expression match {
      case logicalExpression: LogicalExpression => logicalExpression
      case calculatorExpression: CalculatorExpression => calculatorExpression
      case numberValue: NumberValue => numberValue
      case quotedStringValue: QuotedStringValue => quotedStringValue
      case variable: Variable => variable
      case methodCall: MethodCall =>
        methodCall.copy(
          expression = injectLinksIntoExpression(methodCall.expression, typeMapping),
          parameters = methodCall.parameters.map(injectLinksIntoExpression(_, typeMapping)),
          generics = methodCall.generics.map(injectLinksIntoTypeReference(_, typeMapping))
        )
      case attributeCall: AttributeCall =>
        attributeCall.copy(
          expression = injectLinksIntoExpression(attributeCall.expression, typeMapping)
        )
      case combinedExpression: CombinedExpression =>
        combinedExpression.copy(
          parts = combinedExpression.parts.map(injectLinksIntoExpression(_, typeMapping))
        )
      case condition: Condition =>
        condition.copy(
          condition = injectLinksIntoExpression(condition.condition, typeMapping),
          onTrue = injectLinksIntoExpression(condition.onTrue, typeMapping),
          onFalse = condition.onFalse.map(injectLinksIntoExpression(_, typeMapping))
        )
      case lambdaExpression: LambdaExpression =>
        lambdaExpression.copy(
          expression = injectLinksIntoExpression(lambdaExpression.expression, typeMapping),
          parameterList = lambdaExpression.parameterList.map(injectLinksIntoParameter(_, typeMapping))
        )
    }
  }

  private def getLink(name: String, typeMapping: TypeMapping): String = {
    typeMapping.getOrElse(name, name)
  }
}
