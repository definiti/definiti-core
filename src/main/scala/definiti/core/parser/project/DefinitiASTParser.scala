package definiti.core.parser.project

import definiti.core._
import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.ParserUtils._
import definiti.core.utils.StringUtils

import scala.collection.mutable.ListBuffer

private[core] object DefinitiASTParser {
  def definitiContextToAST(context: DefinitiContext): RootFile = {
    val verifications = ListBuffer[Verification]()
    val classDefinitions = ListBuffer[ClassDefinition]()
    val namedFunctions = ListBuffer[NamedFunction]()

    scalaSeq(context.toplevel()).foreach { element =>
      def appendIfDefined[A, B](element: A, buffer: ListBuffer[B], transformer: A => B) = {
        if (element != null) {
          buffer.append(transformer(element))
        }
      }
      appendIfDefined(element.verification(), verifications, processVerification)
      appendIfDefined(element.definedType(), classDefinitions, processDefinedType)
      appendIfDefined(element.aliasType(), classDefinitions, processAliasType)
      appendIfDefined(element.namedFunction(), namedFunctions, processNamedFunction)
    }

    RootFile(
      packageName = extractPackageName(context),
      imports = extractImports(context),
      verifications = List(verifications: _*),
      classDefinitions = List(classDefinitions: _*),
      namedFunctions = List(namedFunctions: _*)
    )
  }

  def processVerification(context: VerificationContext): Verification = {
    Verification(
      name = context.verificationName.getText,
      packageName = NOT_DEFINED,
      message = extractStringContent(context.verificationMessage.getText),
      function = processFunction(context.function()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      getRangeFromContext(context)
    )
  }

  def processDefinedType(context: DefinedTypeContext): DefinedType = {
    val typeName = context.typeName.getText
    DefinedType(
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
      message = Option(context.message).map(_.getText),
      range = getRangeFromContext(context)
    )
  }

  def processTypeVerification(context: TypeVerificationContext, typeName: String): TypeVerification = {
    TypeVerification(
      extractStringContent(context.verificationMessage.getText),
      processTypeVerificationFunction(context.typeVerificationFunction(), typeName),
      getRangeFromContext(context)
    )
  }

  def processTypeVerificationFunction(context: TypeVerificationFunctionContext, typeName: String): DefinedFunction = {
    val parameters = Seq(ParameterDefinition(
      name = context.IDENTIFIER().getText,
      typeReference = TypeReference(typeName, Seq.empty),
      range = getRangeFromTerminalNode(context.IDENTIFIER())
    ))
    DefinedFunction(
      parameters = parameters,
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Seq.empty,
      getRangeFromContext(context)
    )
  }

  def processAliasType(context: AliasTypeContext): AliasType = {
    AliasType(
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

  def processNamedFunction(context: NamedFunctionContext): NamedFunction = {
    NamedFunction(
      name = context.name.getText,
      packageName = NOT_DEFINED,
      function = processFunction(context.function()),
      range = getRangeFromContext(context)
    )
  }

  def processFunction(context: FunctionContext): DefinedFunction = {
    val parameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter)
    DefinedFunction(
      parameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter),
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      getRangeFromContext(context)
    )
  }

  def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = TypeReference(context.parameterType.getText, processGenericTypeList(context.genericTypeList())),
      getRangeFromContext(context)
    )
  }

  def processChainedExpression(context: ChainedExpressionContext): Expression = {
    scalaSeq(context.expression()) match {
      case head :: Nil => processExpression(head)
      case expressions => CombinedExpression(expressions.map(processExpression), getRangeFromContext(context))
    }
  }

  def processExpression(context: ExpressionContext): Expression = {
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

  def processParenthesisExpression(context: ExpressionContext): Expression = {
    processExpression(context.parenthesis)
  }

  def processMethodCallExpression(context: ExpressionContext): Expression = {
    MethodCall(
      expression = processExpression(context.methodExpression),
      method = context.methodName.getText,
      parameters = Option(context.methodExpressionParameters) map { methodExpressionParameters =>
        scalaSeq(methodExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.genericTypeList()),
      getRangeFromContext(context)
    )
  }

  def processAttributeCallExpression(context: ExpressionContext): Expression = {
    AttributeCall(
      expression = processExpression(context.attributeExpression),
      attribute = context.attributeName.getText,
      getRangeFromContext(context)
    )
  }

  def processNotExpression(context: ExpressionContext): Expression = {
    Not(processExpression(context.notExpression), getRangeFromContext(context))
  }

  def processLeftRightExpression(context: ExpressionContext): Expression = {
    val left = processExpression(context.leftExpression)
    val right = processExpression(context.rightExpression)
    context.operator.getText match {
      case "*" => Time(left, right, getRangeFromContext(context))
      case "/" => Divide(left, right, getRangeFromContext(context))
      case "%" => Modulo(left, right, getRangeFromContext(context))
      case "+" => Plus(left, right, getRangeFromContext(context))
      case "-" => Minus(left, right, getRangeFromContext(context))
      case "==" => Equal(left, right, getRangeFromContext(context))
      case "!=" => NotEqual(left, right, getRangeFromContext(context))
      case "<" => Lower(left, right, getRangeFromContext(context))
      case "<=" => LowerOrEqual(left, right, getRangeFromContext(context))
      case ">" => Upper(left, right, getRangeFromContext(context))
      case ">=" => UpperOrEqual(left, right, getRangeFromContext(context))
      case "&&" => And(left, right, getRangeFromContext(context))
      case "||" => Or(left, right, getRangeFromContext(context))
    }
  }

  def processBooleanExpression(context: ExpressionContext): Expression = {
    context.booleanExpression.getText match {
      case "true" => BooleanValue(value = true, getRangeFromContext(context))
      case _ => BooleanValue(value = false, getRangeFromContext(context))
    }
  }

  def processNumberExpression(context: ExpressionContext): Expression = {
    NumberValue(BigDecimal(context.numberExpression.getText), getRangeFromContext(context))
  }

  def processStringExpression(context: ExpressionContext): Expression = {
    QuotedStringValue(extractStringContent(context.stringExpression.getText), getRangeFromContext(context))
  }

  def processReferenceExpression(context: ExpressionContext): Expression = {
    Reference(
      name = context.referenceExpression.getText,
      range = getRangeFromContext(context)
    )
  }

  def processConditionExpression(context: ExpressionContext): Expression = {
    Condition(
      condition = processExpression(context.conditionExpression),
      onTrue = processChainedExpression(context.conditionIfBody),
      onFalse = Option(context.conditionElseBody).map(processChainedExpression),
      getRangeFromContext(context)
    )
  }

  def processLambdaExpression(context: ExpressionContext): Expression = {
    val lambdaParameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter)
    LambdaExpression(
      parameterList = lambdaParameters,
      expression = processExpression(context.lambdaExpression),
      range = getRangeFromContext(context)
    )
  }

  def processFunctionCall(context: ExpressionContext): Expression = {
    FunctionCall(
      name = context.functionName.getText,
      parameters = Option(context.functionExpressionParameters) map { functionExpressionParameters =>
        scalaSeq(functionExpressionParameters.expression()).map(processExpression)
      } getOrElse Seq.empty,
      generics = processGenericTypeList(context.functionGenerics),
      range = getRangeFromContext(context)
    )
  }

  def processGenericTypeList(context: GenericTypeListContext): Seq[TypeReference] = {
    if (context != null) {
      scalaSeq(context.genericType()).map { genericTypeContext =>
        TypeReference(
          genericTypeContext.IDENTIFIER().getText,
          processGenericTypeList(genericTypeContext.genericTypeList())
        )
      }
    } else {
      Seq()
    }
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
}
