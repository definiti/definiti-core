package definiti.core.parser.project

import definiti.core.Configuration
import definiti.core.ast.LogicalOperator.{apply => _}
import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils.scalaSeq
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

  def parse(context: DefinitiContext): PureRootFile = {
    val verifications = ListBuffer[PureVerification]()
    val classDefinitions = ListBuffer[PureClassDefinition]()
    val namedFunctions = ListBuffer[PureNamedFunction]()
    val contexts = ListBuffer[PureExtendedContext[_]]()

    scalaSeq(context.toplevel()).foreach { element =>
      appendIfDefined(element.verification(), verifications, processVerification)
      appendIfDefined(element.definedType(), classDefinitions, processDefinedType)
      appendIfDefined(element.aliasType(), classDefinitions, processAliasType)
      appendIfDefined(element.enumType(), classDefinitions, processEnum)
      appendIfDefined(element.namedFunction(), namedFunctions, processNamedFunction)
      Option(element.context()).foreach { internalContext =>
        processContext(internalContext) match {
          case Some(parsedContext) => contexts.append(parsedContext)
          case None => println(s"No plugin set for context: ${internalContext.IDENTIFIER().getText}")
        }
      }
    }

    PureRootFile(
      packageName = packageName,
      imports = imports,
      verifications = List(verifications: _*),
      classDefinitions = List(classDefinitions: _*),
      namedFunctions = List(namedFunctions: _*),
      contexts = List(contexts: _*)
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

  private def processVerification(context: VerificationContext): PureVerification = {
    PureVerification(
      name = context.verificationName.getText,
      packageName = packageName,
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

  private def processFunction(context: FunctionContext): PureDefinedFunction = {
    PureDefinedFunction(
      parameters = processParameterListDefinition(context.parameterListDefinition()),
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processDefinedType(context: DefinedTypeContext): PureDefinedType = {
    val typeName = context.typeName.getText
    val generics = processGenericTypeListDefinition(context.genericTypeList())
    PureDefinedType(
      name = typeName,
      packageName = packageName,
      genericTypes = generics,
      parameters = Option(context.parameterListDefinition).map(processParameterListDefinition).getOrElse(Seq.empty),
      attributes = scalaSeq(context.attributeDefinition()).map(processAttributeDefinition),
      verifications = scalaSeq(context.typeVerification()).map(processTypeVerification(_, typeName, generics)),
      inherited = processVerifyingList(context.verifyingList()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processAttributeDefinition(context: AttributeDefinitionContext): PureAttributeDefinition = {
    PureAttributeDefinition(
      name = context.attributeName.getText,
      typeDeclaration = processTypeDeclaration(context.typeDeclaration),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      verifications = processVerifyingList(context.verifyingList()),
      location = getLocationFromContext(context)
    )
  }

  private def processVerifyingList(verifyingListContext: VerifyingListContext): Seq[PureVerificationReference] = {
    if (verifyingListContext != null) {
      scalaSeq(verifyingListContext.verifying()).map(processVerifying)
    } else {
      Seq.empty
    }
  }

  private def processVerifying(context: VerifyingContext): PureVerificationReference = {
    PureVerificationReference(
      verificationName = identifierWithImport(context.verificationName),
      parameters = Option(context.atomicExpressionList).map(processAtomicExpressionList).getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processAtomicExpressionList(context: AtomicExpressionListContext): Seq[PureAtomicExpression] = {
    scalaSeq(context.atomicExpression()).map(processAtomicExpression)
  }

  private def processTypeVerification(context: TypeVerificationContext, typeName: String, generics: Seq[String]): PureTypeVerification = {
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

  private def processAtomicTypeVerification(context: AtomicTypeVerificationContext, typeName: String, generics: Seq[String]): PureTypeVerification = {
    PureAtomicTypeVerification(
      processVerificationMessage(context.verificationMessage),
      processAtomicTypeVerificationFunction(context.typeVerificationFunction(), typeName, generics),
      location = getLocationFromContext(context)
    )
  }

  private def processAtomicTypeVerificationFunction(context: TypeVerificationFunctionContext, typeName: String, generics: Seq[String]): PureDefinedFunction = {
    val parameters = Seq(ParameterDefinition(
      name = context.IDENTIFIER().getText,
      typeReference = TypeReference(identifierWithImport(typeName), generics.map(TypeReference(_, Seq.empty))),
      location = Location(file, getRangeFromTerminalNode(context.IDENTIFIER()))
    ))
    PureDefinedFunction(
      parameters = parameters,
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Seq.empty,
      location = getLocationFromContext(context)
    )
  }

  private def processDependentTypeVerification(context: DependentTypeVerificationContext, typeName: String, generics: Seq[String]): PureTypeVerification = {
    PureDependentTypeVerification(
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

  private def processDependantTypeVerificationFunction(context: TypeVerificationFunctionContext, dependentParameters: Seq[ParameterDefinition], typeName: String, generics: Seq[String]): PureDefinedFunction = {
    val parameters = Seq(ParameterDefinition(
      name = context.IDENTIFIER().getText,
      typeReference = TypeReference(identifierWithImport(typeName), generics.map(TypeReference(_, Seq.empty))),
      location = Location(file, getRangeFromTerminalNode(context.IDENTIFIER()))
    ))
    PureDefinedFunction(
      parameters = parameters ++ dependentParameters,
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Seq.empty,
      location = getLocationFromContext(context)
    )
  }

  private def processAliasType(context: AliasTypeContext): PureAliasType = {
    val generics = processGenericTypeListDefinition(context.genericTypes)
    PureAliasType(
      name = context.typeName.getText,
      packageName = packageName,
      genericTypes = generics,
      parameters = Option(context.parameterListDefinition).map(processParameterListDefinition).getOrElse(Seq.empty),
      alias = processTypeDeclaration(context.typeDeclaration),
      verifications = extractAliasTypeVerifications(context.aliasTypeBody(), context.typeName.getText, generics),
      inherited = processVerifyingList(context.verifyingList()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processTypeDeclaration(context: TypeDeclarationContext): PureTypeDeclaration = {
    PureTypeDeclaration(
      typeName = identifierWithImport(context.name),
      genericTypes = Option(context.typeDeclarationList).map(processTypeDeclarationList).getOrElse(Seq.empty),
      parameters = Option(context.atomicExpressionList).map(processAtomicExpressionList).getOrElse(Seq.empty),
      location = getLocationFromContext(context)
    )
  }

  private def processTypeDeclarationList(context: TypeDeclarationListContext): Seq[PureTypeDeclaration] = {
    scalaSeq(context.typeDeclaration).map(processTypeDeclaration)
  }

  private def extractAliasTypeVerifications(context: AliasTypeBodyContext, typeName: String, generics: Seq[String]): Seq[PureTypeVerification] = {
    if (context == null) {
      Seq.empty
    } else {
      scalaSeq(context.typeVerification()).map(processTypeVerification(_, typeName, generics))
    }
  }

  private def processEnum(context: EnumTypeContext): PureEnum = {
    PureEnum(
      name = context.typeName.getText,
      packageName = packageName,
      cases = scalaSeq(context.enumCase()).map(processEnumCase),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processEnumCase(context: EnumCaseContext): PureEnumCase = {
    PureEnumCase(
      name = context.IDENTIFIER().getText,
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      location = getLocationFromContext(context)
    )
  }

  private def processNamedFunction(context: NamedFunctionContext): PureNamedFunction = {
    PureNamedFunction(
      name = context.name.getText,
      packageName = packageName,
      parameters = processParameterListDefinition(context.parameterListDefinition()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      returnType = processGenericType(context.genericType()),
      body = processNamedFunctionBody(context.namedFunctionBody()),
      location = getLocationFromContext(context)
    )
  }

  private def processNamedFunctionBody(context: NamedFunctionBodyContext): PureExpression = {
    if (context.chainedExpression() != null) {
      processChainedExpression(context.chainedExpression())
    } else {
      processExpression(context.expression())
    }
  }

  private def processChainedExpression(context: ChainedExpressionContext): PureExpression = {
    scalaSeq(context.expression()) match {
      case head :: Nil => processExpression(head)
      case expressions => PureCombinedExpression(expressions.map(processExpression), getLocationFromContext(context))
    }
  }

  private def processExpression(context: ExpressionContext): PureExpression = {
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

  private def processParenthesisExpression(context: ExpressionContext): PureExpression = {
    processExpression(context.parenthesis)
  }

  private def processOkExpression(context: ExpressionContext): PureExpression = {
    PureOkValue(getLocationFromContext(context))
  }

  private def processKoExpression(context: ExpressionContext): PureExpression = {
    PureKoValue(
      parameters = Option(context.koExpressionParameters) map { koExpressionParameters =>
        scalaSeq(koExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      location = getLocationFromContext(context)
    )
  }

  private def processMethodCallExpression(context: ExpressionContext): PureExpression = {
    PureMethodCall(
      expression = processExpression(context.methodExpression),
      method = context.methodName.getText,
      parameters = Option(context.methodExpressionParameters) map { methodExpressionParameters =>
        scalaSeq(methodExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.genericTypeList()),
      location = getLocationFromContext(context)
    )
  }

  private def processAttributeCallExpression(context: ExpressionContext): PureExpression = {
    PureAttributeCall(
      expression = processExpression(context.attributeExpression),
      attribute = context.attributeName.getText,
      location = getLocationFromContext(context)
    )
  }

  private def processNotExpression(context: ExpressionContext): PureExpression = {
    PureNot(processExpression(context.notExpression), getLocationFromContext(context))
  }

  private def processLeftRightExpression(context: ExpressionContext): PureExpression = {
    import CalculatorOperator._
    import LogicalOperator._
    val left = processExpression(context.leftExpression)
    val right = processExpression(context.rightExpression)
    context.operator.getText match {
      case "*" => PureCalculatorExpression(Time, left, right, getLocationFromContext(context))
      case "/" => PureCalculatorExpression(Divide, left, right, getLocationFromContext(context))
      case "%" => PureCalculatorExpression(Modulo, left, right, getLocationFromContext(context))
      case "+" => PureCalculatorExpression(Plus, left, right, getLocationFromContext(context))
      case "-" => PureCalculatorExpression(Minus, left, right, getLocationFromContext(context))
      case "==" => PureLogicalExpression(Equal, left, right, getLocationFromContext(context))
      case "!=" => PureLogicalExpression(NotEqual, left, right, getLocationFromContext(context))
      case "<" => PureLogicalExpression(Lower, left, right, getLocationFromContext(context))
      case "<=" => PureLogicalExpression(LowerOrEqual, left, right, getLocationFromContext(context))
      case ">" => PureLogicalExpression(Upper, left, right, getLocationFromContext(context))
      case ">=" => PureLogicalExpression(UpperOrEqual, left, right, getLocationFromContext(context))
      case "&&" => PureLogicalExpression(And, left, right, getLocationFromContext(context))
      case "||" => PureLogicalExpression(Or, left, right, getLocationFromContext(context))
    }
  }

  private def processAtomicExpression(context: AtomicExpressionContext): PureAtomicExpression = {
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

  private def processBooleanExpression(context: AtomicExpressionContext): PureAtomicExpression = {
    context.booleanExpression.getText match {
      case "true" => PureBooleanValue(value = true, getLocationFromContext(context))
      case _ => PureBooleanValue(value = false, getLocationFromContext(context))
    }
  }

  private def processNumberExpression(context: AtomicExpressionContext): PureAtomicExpression = {
    PureNumberValue(BigDecimal(context.numberExpression.getText), getLocationFromContext(context))
  }

  private def processStringExpression(context: AtomicExpressionContext): PureAtomicExpression = {
    PureQuotedStringValue(extractStringContent(context.stringExpression.getText), getLocationFromContext(context))
  }

  private def processReferenceExpression(context: AtomicExpressionContext): PureAtomicExpression = {
    PureReference(
      name = context.referenceExpression.getText,
      location = getLocationFromContext(context)
    )
  }

  private def processConditionExpression(context: ExpressionContext): PureExpression = {
    PureCondition(
      condition = processExpression(context.conditionExpression),
      onTrue = processChainedExpression(context.conditionIfBody),
      onFalse = Option(context.conditionElseBody).map(processChainedExpression),
      location = getLocationFromContext(context)
    )
  }

  private def processLambdaExpression(context: ExpressionContext): PureExpression = {
    PureLambdaExpression(
      parameterList = processParameterListDefinition(context.parameterListDefinition()),
      expression = processExpression(context.lambdaExpression),
      location = getLocationFromContext(context)
    )
  }

  private def processFunctionCall(context: ExpressionContext): PureExpression = {
    PureFunctionCall(
      name = identifierWithImport(context.functionName),
      parameters = Option(context.functionExpressionParameters) map { functionExpressionParameters =>
        scalaSeq(functionExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.functionGenerics),
      location = getLocationFromContext(context)
    )
  }

  private def processGenericTypeListDefinition(context: GenericTypeListContext): Seq[String] = {
    Option(context)
      .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
      .getOrElse(Seq())
  }

  private def processContext(context: ContextContext): Option[PureExtendedContext[_]] = {
    val contextName = context.IDENTIFIER().getText
    configuration.contexts
      .find(_.contextName == contextName)
      .map { contextPlugin =>
        val contextContent = context.contextContent()
        val contentInterval = new Interval(contextContent.getStart.getStartIndex, contextContent.getStop.getStopIndex)
        val content = contextContent.getStart.getInputStream.getText(contentInterval)
        val location = getLocationFromContext(contextContent)
        PureExtendedContext(contextName, contextPlugin.parse(content, packageName, imports, location), location)
      }
  }

  private def processParameterListDefinition(context: ParameterListDefinitionContext): Seq[ParameterDefinition] = {
    scalaSeq(context.parameterDefinition()).map(processParameter)
  }

  private def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = processTypeReference(context.typeReference()),
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
