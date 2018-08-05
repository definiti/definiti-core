package definiti.core.parser.project

import definiti.common.ast.LogicalOperator.{apply => _}
import definiti.common.ast._
import definiti.common.utils.CollectionUtils.scalaSeq
import definiti.common.utils.StringUtils
import definiti.core.Configuration
import definiti.core.parser.antlr.DefinitiParser._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.misc.Interval

import scala.collection.mutable.ListBuffer

private[core] class DefinitiFileASTParser(
  sourceFile: String,
  configuration: Configuration,
  packageName: String,
  imports: Map[String, String]
) extends CommonParser {
  val file: String = sourceFile.replaceAllLiterally("\\", "/")

  def parse(context: DefinitiContext): Namespace = {
    val elements = ListBuffer[NamespaceElement]()

    scalaSeq(context.toplevel()).foreach { element =>
      appendIfDefined(element.verification(), elements, processVerification)
      appendIfDefined(element.definedType(), elements, processDefinedType)
      appendIfDefined(element.aliasType(), elements, processAliasType)
      appendIfDefined(element.enumType(), elements, processEnum)
      appendIfDefined(element.namedFunction(), elements, processNamedFunction)
      Option(element.context()).foreach { internalContext =>
        processContext(internalContext) match {
          case Some(parsedContext) => elements.append(parsedContext)
          case None => println(s"No plugin set for context: ${internalContext.IDENTIFIER().getText}")
        }
      }
    }

    Namespace(
      name = StringUtils.lastPart(packageName),
      fullName = packageName,
      elements = List(elements: _*)
    )
  }

  private def extractTopLevelNames(context: DefinitiContext): Seq[String] = {
    val topLevelNames = ListBuffer[String]()
    scalaSeq(context.toplevel()).foreach { element =>
      appendIfDefined(element.verification(), topLevelNames, (c: VerificationContext) => c.verificationName.getText)
      appendIfDefined(element.definedType(), topLevelNames, (c: DefinedTypeContext) => c.typeName.getText)
      appendIfDefined(element.aliasType(), topLevelNames, (c: AliasTypeContext) => c.typeName.getText)
      appendIfDefined(element.enumType(), topLevelNames, (c: EnumTypeContext) => c.typeName.getText)
      appendIfDefined(element.namedFunction(), topLevelNames, (c: NamedFunctionContext) => c.name.getText)
    }
    List(topLevelNames: _*)
  }

  private def processVerification(context: VerificationContext): Verification = {
    Verification(
      name = context.verificationName.getText,
      fullName = StringUtils.canonical(packageName, context.verificationName.getText),
      parameters = Option(context.parameterListDefinition).map(processParameterListDefinition).getOrElse(Seq.empty),
      message = processVerificationMessage(context.verificationMessage()),
      function = processFunction(context.function()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processVerificationMessage(context: VerificationMessageContext): VerificationMessage = {
    if (context.literal != null) {
      LiteralMessage(
        message = extractStringContent(context.literal.getText),
        location = getLocationFromContext(context)
      )
    } else {
      TypedMessage(
        message = extractStringContent(context.message.getText),
        types = scalaSeq(context.typeReference()).map(processTypeReference),
        location = getLocationFromContext(context)
      )
    }
  }

  private def processFunction(context: FunctionContext): DefinedFunction = {
    DefinedFunction(
      parameters = processParameterListDefinition(context.parameterListDefinition()),
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processDefinedType(context: DefinedTypeContext): DefinedType = {
    val typeName = context.typeName.getText
    val generics = processGenericTypeListDefinition(context.genericTypeList())
    DefinedType(
      name = typeName,
      fullName = StringUtils.canonical(packageName, typeName),
      genericTypes = generics,
      parameters = Option(context.parameterListDefinition).map(processParameterListDefinition).getOrElse(Seq.empty),
      attributes = scalaSeq(context.attributeDefinition()).map(processAttributeDefinition),
      verifications = scalaSeq(context.typeVerification()).map(processTypeVerification(_, typeName, generics)),
      inherited = processVerifyingList(context.verifyingList()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processAttributeDefinition(context: AttributeDefinitionContext): AttributeDefinition = {
    val attributeName = context.attributeName.getText
    val typeDeclaration = Option(context.typeDeclaration)
        .map(processTypeDeclaration)
        .getOrElse(TypeDeclaration(attributeName.capitalize, Seq.empty, Seq.empty, getLocationFromToken(context.attributeName)))
    AttributeDefinition(
      name = attributeName,
      typeDeclaration = typeDeclaration,
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      verifications = processVerifyingList(context.verifyingList()),
      typeName = Option(context.attributeTypeName).map(_.getText),
      location = getLocationFromContext(context)
    )
  }

  private def processVerifyingList(verifyingListContext: VerifyingListContext): Seq[VerificationReference] = {
    if (verifyingListContext != null) {
      scalaSeq(verifyingListContext.verifying()).map(processVerifying)
    } else {
      Seq.empty
    }
  }

  private def processVerifying(context: VerifyingContext): VerificationReference = {
    VerificationReference(
      verificationName = identifierWithImport(context.verificationName),
      parameters = Option(context.atomicExpressionList).map(processAtomicExpressionList).getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processAtomicExpressionList(context: AtomicExpressionListContext): Seq[AtomicExpression] = {
    scalaSeq(context.atomicExpression()).map(processAtomicExpression)
  }

  private def processTypeVerification(context: TypeVerificationContext, typeName: String, generics: Seq[String]): TypeVerification = {
    if (context.atomicTypeVerification() != null) {
      processAtomicTypeVerification(context.atomicTypeVerification(), typeName, generics)
    } else if (context.dependentTypeVerification() != null) {
      processDependentTypeVerification(context.dependentTypeVerification(), typeName, generics)
    } else {
      // This exception exists to remind us to implement expression processing when we add one
      // This should never happen in production code.
      throw new RuntimeException(s"TypeVerification ${context.getText} was not processed")
    }
  }

  private def processAtomicTypeVerification(context: AtomicTypeVerificationContext, typeName: String, generics: Seq[String]): TypeVerification = {
    AtomicTypeVerification(
      processVerificationMessage(context.verificationMessage),
      processAtomicTypeVerificationFunction(context.typeVerificationFunction(), typeName, generics),
      location = getLocationFromContext(context)
    )
  }

  private def processAtomicTypeVerificationFunction(context: TypeVerificationFunctionContext, typeName: String, generics: Seq[String]): DefinedFunction = {
    val parameters = Seq(ParameterDefinition(
      name = context.IDENTIFIER().getText,
      typeReference = TypeReference(identifierWithImport(typeName), generics.map(TypeReference(_, Seq.empty))),
      location = Location(file, getRangeFromTerminalNode(context.IDENTIFIER()))
    ))
    DefinedFunction(
      parameters = parameters,
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Seq.empty,
      location = getLocationFromContext(context)
    )
  }

  private def processDependentTypeVerification(context: DependentTypeVerificationContext, typeName: String, generics: Seq[String]): TypeVerification = {
    DependentTypeVerification(
      name = context.verificationName.getText,
      processVerificationMessage(context.verificationMessage),
      processDependantTypeVerificationFunction(
        context = context.typeVerificationFunction(),
        dependentParameters = processParameterListDefinition(context.parameterListDefinition()),
        typeName = typeName,
        generics = generics
      ),
      location = getLocationFromContext(context)
    )
  }

  private def processDependantTypeVerificationFunction(context: TypeVerificationFunctionContext, dependentParameters: Seq[ParameterDefinition], typeName: String, generics: Seq[String]): DefinedFunction = {
    val parameters = Seq(ParameterDefinition(
      name = context.IDENTIFIER().getText,
      typeReference = TypeReference(identifierWithImport(typeName), generics.map(TypeReference(_, Seq.empty))),
      location = Location(file, getRangeFromTerminalNode(context.IDENTIFIER()))
    ))
    DefinedFunction(
      parameters = parameters ++ dependentParameters,
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Seq.empty,
      location = getLocationFromContext(context)
    )
  }

  private def processAliasType(context: AliasTypeContext): AliasType = {
    val generics = processGenericTypeListDefinition(context.genericTypes)
    AliasType(
      name = context.typeName.getText,
      fullName = StringUtils.canonical(packageName, context.typeName.getText),
      genericTypes = generics,
      parameters = Option(context.parameterListDefinition).map(processParameterListDefinition).getOrElse(Seq.empty),
      alias = processTypeDeclaration(context.typeDeclaration),
      verifications = extractAliasTypeVerifications(context.aliasTypeBody(), context.typeName.getText, generics),
      inherited = processVerifyingList(context.verifyingList()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processTypeDeclaration(context: TypeDeclarationContext): TypeDeclaration = {
    TypeDeclaration(
      typeName = identifierWithImport(context.name),
      genericTypes = Option(context.typeDeclarationList).map(processTypeDeclarationList).getOrElse(Seq.empty),
      parameters = Option(context.atomicExpressionList).map(processAtomicExpressionList).getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processTypeDeclarationList(context: TypeDeclarationListContext): Seq[TypeDeclaration] = {
    scalaSeq(context.typeDeclaration).map(processTypeDeclaration)
  }

  private def extractAliasTypeVerifications(context: AliasTypeBodyContext, typeName: String, generics: Seq[String]): Seq[TypeVerification] = {
    if (context == null) {
      Seq.empty
    } else {
      scalaSeq(context.typeVerification()).map(processTypeVerification(_, typeName, generics))
    }
  }

  private def processEnum(context: EnumTypeContext): Enum = {
    Enum(
      name = context.typeName.getText,
      fullName = StringUtils.canonical(packageName, context.typeName.getText),
      cases = scalaSeq(context.enumCase()).map(processEnumCase),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processEnumCase(context: EnumCaseContext): EnumCase = {
    EnumCase(
      name = context.IDENTIFIER().getText,
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processNamedFunction(context: NamedFunctionContext): NamedFunction = {
    NamedFunction(
      name = context.name.getText,
      fullName = StringUtils.canonical(packageName, context.name.getText),
      parameters = processParameterListDefinition(context.parameterListDefinition()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      returnType = processGenericType(context.genericType()),
      body = processNamedFunctionBody(context.namedFunctionBody()),
      location = getLocationFromContext(context)
    )
  }

  private def processNamedFunctionBody(context: NamedFunctionBodyContext): Expression = {
    if (context.chainedExpression() != null) {
      processChainedExpression(context.chainedExpression())
    } else {
      processExpression(context.expression())
    }
  }

  private def processChainedExpression(context: ChainedExpressionContext): Expression = {
    scalaSeq(context.expression()) match {
      case head :: Nil => processExpression(head)
      case expressions => CombinedExpression(
        parts = expressions.map(processExpression),
        returnType = Unset,
        location = getLocationFromContext(context)
      )
    }
  }

  private def processExpression(context: ExpressionContext): Expression = {
    if (context.parenthesis != null) {
      processParenthesisExpression(context)
    } else if (context.OK() != null) {
      processOkExpression(context)
    } else if (context.KO() != null) {
      processKoExpression(context)
    } else if (context.methodName != null) {
      processMethodCallExpression(context)
    } else if (context.attributeName != null) {
      processAttributeCallExpression(context)
    } else if (context.notExpression != null) {
      processNotExpression(context)
    } else if (context.leftExpression != null) {
      processLeftRightExpression(context)
    } else if (context.atomicExpression != null) {
      processAtomicExpression(context.atomicExpression)
    } else if (context.conditionExpression != null) {
      processConditionExpression(context)
    } else if (context.lambdaExpression != null) {
      processLambdaExpression(context)
    } else if (context.functionName != null) {
      processFunctionCall(context)
    } else {
      // This exception exists to remind us to implement expression processing when we add one
      // This should never happen in production code.
      throw new RuntimeException(s"Expression ${context.getText} was not processed")
    }
  }

  private def processParenthesisExpression(context: ExpressionContext): Expression = {
    processExpression(context.parenthesis)
  }

  private def processOkExpression(context: ExpressionContext): Expression = {
    OkValue(
      returnType = Unset,
      getLocationFromContext(context)
    )
  }

  private def processKoExpression(context: ExpressionContext): Expression = {
    KoValue(
      parameters = Option(context.koExpressionParameters) map { koExpressionParameters =>
        scalaSeq(koExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processMethodCallExpression(context: ExpressionContext): Expression = {
    MethodCall(
      expression = processExpression(context.methodExpression),
      method = context.methodName.getText,
      parameters = Option(context.methodExpressionParameters) map { methodExpressionParameters =>
        scalaSeq(methodExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.genericTypeList()),
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processAttributeCallExpression(context: ExpressionContext): Expression = {
    AttributeCall(
      expression = processExpression(context.attributeExpression),
      attribute = context.attributeName.getText,
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processNotExpression(context: ExpressionContext): Expression = {
    Not(
      inner = processExpression(context.notExpression),
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processLeftRightExpression(context: ExpressionContext): Expression = {
    import definiti.common.ast.CalculatorOperator._
    import definiti.common.ast.LogicalOperator._
    val left = processExpression(context.leftExpression)
    val right = processExpression(context.rightExpression)
    context.operator.getText match {
      case "*" => CalculatorExpression(Time, left, right, Unset, getLocationFromContext(context))
      case "/" => CalculatorExpression(Divide, left, right, Unset, getLocationFromContext(context))
      case "%" => CalculatorExpression(Modulo, left, right, Unset, getLocationFromContext(context))
      case "+" => CalculatorExpression(Plus, left, right, Unset, getLocationFromContext(context))
      case "-" => CalculatorExpression(Minus, left, right, Unset, getLocationFromContext(context))
      case "==" => LogicalExpression(Equal, left, right, Unset, getLocationFromContext(context))
      case "!=" => LogicalExpression(NotEqual, left, right, Unset, getLocationFromContext(context))
      case "<" => LogicalExpression(Lower, left, right, Unset, getLocationFromContext(context))
      case "<=" => LogicalExpression(LowerOrEqual, left, right, Unset, getLocationFromContext(context))
      case ">" => LogicalExpression(Upper, left, right, Unset, getLocationFromContext(context))
      case ">=" => LogicalExpression(UpperOrEqual, left, right, Unset, getLocationFromContext(context))
      case "&&" => LogicalExpression(And, left, right, Unset, getLocationFromContext(context))
      case "||" => LogicalExpression(Or, left, right, Unset, getLocationFromContext(context))
    }
  }

  private def processAtomicExpression(context: AtomicExpressionContext): AtomicExpression = {
    if (context.booleanExpression != null) {
      processBooleanExpression(context)
    } else if (context.numberExpression != null) {
      processNumberExpression(context)
    } else if (context.stringExpression != null) {
      processStringExpression(context)
    } else if (context.referenceExpression != null) {
      processReferenceExpression(context)
    } else {
      // This exception exists to remind us to implement expression processing when we add one
      // This should never happen in production code.
      throw new RuntimeException(s"Expression ${context.getText} was not processed")
    }
  }

  private def processBooleanExpression(context: AtomicExpressionContext): AtomicExpression = {
    context.booleanExpression.getText match {
      case "true" => BooleanValue(value = true, Unset, getLocationFromContext(context))
      case _ => BooleanValue(value = false, Unset, getLocationFromContext(context))
    }
  }

  private def processNumberExpression(context: AtomicExpressionContext): AtomicExpression = {
    NumberValue(
      value = BigDecimal(context.numberExpression.getText),
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processStringExpression(context: AtomicExpressionContext): AtomicExpression = {
    QuotedStringValue(
      value = extractStringContent(context.stringExpression.getText),
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processReferenceExpression(context: AtomicExpressionContext): AtomicExpression = {
    Reference(
      name = context.referenceExpression.getText,
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processConditionExpression(context: ExpressionContext): Expression = {
    Condition(
      condition = processExpression(context.conditionExpression),
      onTrue = processChainedExpression(context.conditionIfBody),
      onFalse = Option(context.conditionElseBody).map(processChainedExpression),
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processLambdaExpression(context: ExpressionContext): Expression = {
    LambdaExpression(
      parameterList = processParameterListDefinition(context.parameterListDefinition()),
      expression = processExpression(context.lambdaExpression),
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processFunctionCall(context: ExpressionContext): Expression = {
    FunctionCall(
      name = identifierWithImport(context.functionName),
      parameters = Option(context.functionExpressionParameters) map { functionExpressionParameters =>
        scalaSeq(functionExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.functionGenerics),
      returnType = Unset,
      location = getLocationFromContext(context)
    )
  }

  private def processGenericTypeListDefinition(context: GenericTypeListContext): Seq[String] = {
    Option(context)
      .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
      .getOrElse(Seq())
  }

  private def processContext(context: ContextContext): Option[ExtendedContext[_]] = {
    val contextName = context.IDENTIFIER().getText
    configuration.contexts
      .find(_.contextName == contextName)
      .map { contextPlugin =>
        val contextContent = context.contextContent()
        val contentInterval = new Interval(contextContent.getStart.getStartIndex, contextContent.getStop.getStopIndex)
        val content = contextContent.getStart.getInputStream.getText(contentInterval)
        val location = getLocationFromContext(contextContent)
        ExtendedContext(contextName, contextPlugin.parse(content, packageName, imports, location), location)
      }
  }

  private def processParameterListDefinition(context: ParameterListDefinitionContext): Seq[ParameterDefinition] = {
    scalaSeq(context.parameterDefinition()).map(processParameter)
  }

  private def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    val name = context.parameterName.getText
    val typeReference = Option(context.typeReference())
      .map(processTypeReference)
      .getOrElse(TypeReference(identifierWithImport(name.capitalize)))
    ParameterDefinition(
      name = name,
      typeReference = typeReference,
      location = Location(file, getRangeFromContext(context))
    )
  }

  private def processTypeReference(context: TypeReferenceContext): TypeReference = {
    TypeReference(identifierWithImport(context.name), processGenericTypeList(context.genericTypeList()))
  }

  private def processGenericTypeList(context: GenericTypeListContext): Seq[TypeReference] = {
    if (context != null) {
      scalaSeq(context.genericType()).map(processGenericType)
    } else {
      Seq()
    }
  }

  private def processGenericType(context: GenericTypeContext): TypeReference = {
    TypeReference(
      context.IDENTIFIER().getText,
      processGenericTypeList(context.genericTypeList())
    )
  }

  private def extractStringContent(string: String): String = {
    var temporaryResult = string
    if (temporaryResult.startsWith("\"")) {
      temporaryResult = temporaryResult.substring(1)
    }
    if (temporaryResult.endsWith("\"")) {
      temporaryResult = temporaryResult.substring(0, temporaryResult.length - 1)
    }
    temporaryResult
  }

  private def identifierWithImport(identifier: Token): String = {
    identifierWithImport(identifier.getText)
  }

  private def identifierWithImport(identifier: String): String = {
    imports.getOrElse(identifier, identifier)
  }
}
