package definiti.core.parser.project

import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.StringUtils
import definiti.core.{Configuration, NOT_DEFINED}
import org.antlr.v4.runtime.misc.Interval

import scala.collection.mutable.ListBuffer

private[core] class DefinitiASTParser(configuration: Configuration) extends CommonParser {
  def definitiContextToAST(context: DefinitiContext): PureRootFile = {
    val verifications = ListBuffer[PureVerification]()
    val classDefinitions = ListBuffer[PureClassDefinition]()
    val namedFunctions = ListBuffer[PureNamedFunction]()
    val contexts = ListBuffer[PureExtendedContext[_]]()

    scalaSeq(context.toplevel()).foreach { element =>
      appendIfDefined(element.verification(), verifications, processVerification)
      appendIfDefined(element.definedType(), classDefinitions, processDefinedType)
      appendIfDefined(element.aliasType(), classDefinitions, processAliasType)
      appendIfDefined(element.namedFunction(), namedFunctions, processNamedFunction)
      Option(element.context()).foreach { internalContext =>
        processContext(internalContext) match {
          case Some(parsedContext) => contexts.append(parsedContext)
          case None => println(s"No plugin set for context: ${internalContext.IDENTIFIER().getText}")
        }
      }
    }

    PureRootFile(
      packageName = extractPackageName(context),
      imports = extractImports(context),
      verifications = List(verifications: _*),
      classDefinitions = List(classDefinitions: _*),
      namedFunctions = List(namedFunctions: _*),
      contexts = List(contexts: _*)
    )
  }

  def processVerification(context: VerificationContext): PureVerification = {
    PureVerification(
      name = context.verificationName.getText,
      packageName = NOT_DEFINED,
      message = extractStringContent(context.verificationMessage.getText),
      function = processFunction(context.function()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      getRangeFromContext(context)
    )
  }

  def processFunction(context: FunctionContext): PureDefinedFunction = {
    PureDefinedFunction(
      parameters = processParameterListDefinition(context.parameterListDefinition()),
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      getRangeFromContext(context)
    )
  }

  def processDefinedType(context: DefinedTypeContext): PureDefinedType = {
    val typeName = context.typeName.getText
    PureDefinedType(
      name = typeName,
      packageName = NOT_DEFINED,
      genericTypes = processGenericTypeListDefinition(context.genericTypeList()),
      attributes = scalaSeq(context.attributeDefinition()).map(processAttributeDefinition),
      verifications = scalaSeq(context.typeVerification()).map(processTypeVerification(_, typeName)),
      inherited = processVerifyingList(context.verifyingList()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      range = getRangeFromContext(context)
    )
  }

  def processAttributeDefinition(context: AttributeDefinitionContext): AttributeDefinition = {
    AttributeDefinition(
      name = context.attributeName.getText,
      typeReference = TypeReference(context.attributeType.getText, processGenericTypeList(context.genericTypeList())),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      verifications = processVerifyingList(context.verifyingList()),
      range = getRangeFromContext(context)
    )
  }

  def processVerifyingList(verifyingListContext: VerifyingListContext): Seq[VerificationReference] = {
    if (verifyingListContext != null) {
      scalaSeq(verifyingListContext.verifying()).map(processVerifying)
    } else {
      Seq.empty
    }
  }

  def processVerifying(context: VerifyingContext): VerificationReference = {
    VerificationReference(
      verificationName = context.verificationName.getText,
      message = Option(context.message).map(message => extractStringContent(message.getText)),
      range = getRangeFromContext(context)
    )
  }

  def processTypeVerification(context: TypeVerificationContext, typeName: String): PureTypeVerification = {
    PureTypeVerification(
      extractStringContent(context.verificationMessage.getText),
      processTypeVerificationFunction(context.typeVerificationFunction(), typeName),
      getRangeFromContext(context)
    )
  }

  def processTypeVerificationFunction(context: TypeVerificationFunctionContext, typeName: String): PureDefinedFunction = {
    val parameters = Seq(ParameterDefinition(
      name = context.IDENTIFIER().getText,
      typeReference = TypeReference(typeName, Seq.empty),
      range = getRangeFromTerminalNode(context.IDENTIFIER())
    ))
    PureDefinedFunction(
      parameters = parameters,
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Seq.empty,
      getRangeFromContext(context)
    )
  }

  def processAliasType(context: AliasTypeContext): PureAliasType = {
    PureAliasType(
      name = context.typeName.getText,
      packageName = NOT_DEFINED,
      alias = TypeReference(
        typeName = context.referenceTypeName.getText,
        genericTypes = processGenericTypeList(context.aliasGenericTypes)
      ),
      genericTypes = processGenericTypeListDefinition(context.genericTypes),
      inherited = processVerifyingList(context.verifyingList()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      range = getRangeFromContext(context)
    )
  }

  def processNamedFunction(context: NamedFunctionContext): PureNamedFunction = {
    PureNamedFunction(
      name = context.name.getText,
      packageName = NOT_DEFINED,
      parameters = processParameterListDefinition(context.parameterListDefinition()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      returnType = processGenericType(context.genericType()),
      body = processNamedFunctionBody(context.namedFunctionBody()),
      range = getRangeFromContext(context)
    )
  }

  def processNamedFunctionBody(context: NamedFunctionBodyContext): PureExpression = {
    if (context.chainedExpression() != null) {
      processChainedExpression(context.chainedExpression())
    } else {
      processExpression(context.expression())
    }
  }

  def processChainedExpression(context: ChainedExpressionContext): PureExpression = {
    scalaSeq(context.expression()) match {
      case head :: Nil => processExpression(head)
      case expressions => PureCombinedExpression(expressions.map(processExpression), getRangeFromContext(context))
    }
  }

  def processExpression(context: ExpressionContext): PureExpression = {
    if (context.parenthesis != null) {
      processParenthesisExpression(context)
    } else if (context.methodName != null) {
      processMethodCallExpression(context)
    } else if (context.attributeName != null) {
      processAttributeCallExpression(context)
    } else if (context.notExpression != null) {
      processNotExpression(context)
    } else if (context.leftExpression != null) {
      processLeftRightExpression(context)
    } else if (context.booleanExpression != null) {
      processBooleanExpression(context)
    } else if (context.numberExpression != null) {
      processNumberExpression(context)
    } else if (context.stringExpression != null) {
      processStringExpression(context)
    } else if (context.referenceExpression != null) {
      processReferenceExpression(context)
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

  def processParenthesisExpression(context: ExpressionContext): PureExpression = {
    processExpression(context.parenthesis)
  }

  def processMethodCallExpression(context: ExpressionContext): PureExpression = {
    PureMethodCall(
      expression = processExpression(context.methodExpression),
      method = context.methodName.getText,
      parameters = Option(context.methodExpressionParameters) map { methodExpressionParameters =>
        scalaSeq(methodExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.genericTypeList()),
      getRangeFromContext(context)
    )
  }

  def processAttributeCallExpression(context: ExpressionContext): PureExpression = {
    PureAttributeCall(
      expression = processExpression(context.attributeExpression),
      attribute = context.attributeName.getText,
      getRangeFromContext(context)
    )
  }

  def processNotExpression(context: ExpressionContext): PureExpression = {
    PureNot(processExpression(context.notExpression), getRangeFromContext(context))
  }

  def processLeftRightExpression(context: ExpressionContext): PureExpression = {
    import CalculatorOperator._
    import LogicalOperator._
    val left = processExpression(context.leftExpression)
    val right = processExpression(context.rightExpression)
    context.operator.getText match {
      case "*" => PureCalculatorExpression(Time, left, right, getRangeFromContext(context))
      case "/" => PureCalculatorExpression(Divide, left, right, getRangeFromContext(context))
      case "%" => PureCalculatorExpression(Modulo, left, right, getRangeFromContext(context))
      case "+" => PureCalculatorExpression(Plus, left, right, getRangeFromContext(context))
      case "-" => PureCalculatorExpression(Minus, left, right, getRangeFromContext(context))
      case "==" => PureLogicalExpression(Equal, left, right, getRangeFromContext(context))
      case "!=" => PureLogicalExpression(NotEqual, left, right, getRangeFromContext(context))
      case "<" => PureLogicalExpression(Lower, left, right, getRangeFromContext(context))
      case "<=" => PureLogicalExpression(LowerOrEqual, left, right, getRangeFromContext(context))
      case ">" => PureLogicalExpression(Upper, left, right, getRangeFromContext(context))
      case ">=" => PureLogicalExpression(UpperOrEqual, left, right, getRangeFromContext(context))
      case "&&" => PureLogicalExpression(And, left, right, getRangeFromContext(context))
      case "||" => PureLogicalExpression(Or, left, right, getRangeFromContext(context))
    }
  }

  def processBooleanExpression(context: ExpressionContext): PureExpression = {
    context.booleanExpression.getText match {
      case "true" => PureBooleanValue(value = true, getRangeFromContext(context))
      case _ => PureBooleanValue(value = false, getRangeFromContext(context))
    }
  }

  def processNumberExpression(context: ExpressionContext): PureExpression = {
    PureNumberValue(BigDecimal(context.numberExpression.getText), getRangeFromContext(context))
  }

  def processStringExpression(context: ExpressionContext): PureExpression = {
    PureQuotedStringValue(extractStringContent(context.stringExpression.getText), getRangeFromContext(context))
  }

  def processReferenceExpression(context: ExpressionContext): PureExpression = {
    PureReference(
      name = context.referenceExpression.getText,
      range = getRangeFromContext(context)
    )
  }

  def processConditionExpression(context: ExpressionContext): PureExpression = {
    PureCondition(
      condition = processExpression(context.conditionExpression),
      onTrue = processChainedExpression(context.conditionIfBody),
      onFalse = Option(context.conditionElseBody).map(processChainedExpression),
      getRangeFromContext(context)
    )
  }

  def processLambdaExpression(context: ExpressionContext): PureExpression = {
    PureLambdaExpression(
      parameterList = processParameterListDefinition(context.parameterListDefinition()),
      expression = processExpression(context.lambdaExpression),
      range = getRangeFromContext(context)
    )
  }

  def processFunctionCall(context: ExpressionContext): PureExpression = {
    PureFunctionCall(
      name = context.functionName.getText,
      parameters = Option(context.functionExpressionParameters) map { functionExpressionParameters =>
        scalaSeq(functionExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.functionGenerics),
      range = getRangeFromContext(context)
    )
  }

  def processGenericTypeListDefinition(context: GenericTypeListContext): Seq[String] = {
    Option(context)
      .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
      .getOrElse(Seq())
  }

  def extractPackageName(context: DefinitiContext): String = {
    Option(context.packageName())
      .map(packageNameContext => dottedIdentifierToIdentifier(packageNameContext.dottedIdentifier()))
      .getOrElse("")
  }

  def extractImports(context: DefinitiContext): Map[String, String] = {
    scalaSeq(context.imports())
      .view
      .map(importContext => dottedIdentifierToIdentifier(importContext.dottedIdentifier()))
      .map(fullName => StringUtils.lastPart(fullName, '.') -> fullName)
      .toMap
  }

  def dottedIdentifierToIdentifier(context: DottedIdentifierContext): String = {
    scalaSeq(context.IDENTIFIER()).map(_.getText).mkString(".")
  }

  def processContext(context: ContextContext): Option[PureExtendedContext[_]] = {
    val contextName = context.IDENTIFIER().getText
    configuration.contexts
      .find(_.contextName == contextName)
      .map { contextPlugin =>
        val contextContent = context.contextContent()
        val contentInterval = new Interval(contextContent.getStart.getStartIndex, contextContent.getStop.getStopIndex)
        val content = contextContent.getStart.getInputStream.getText(contentInterval)
        val range = getRangeFromContext(contextContent)
        PureExtendedContext(contextName, contextPlugin.parse(content, range), range)
      }
  }
}
