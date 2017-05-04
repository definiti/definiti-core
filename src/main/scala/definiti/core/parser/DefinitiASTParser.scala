package definiti.core.parser

import definiti.core._
import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.ParserUtils._
import definiti.core.utils.StringUtils

import scala.collection.mutable.ListBuffer

private[parser] case class Scope(variables: Seq[Variable])

private[parser] object Scope {
  val empty = Scope(Seq())
}

private[core] object DefinitiASTParser {
  def definitiContextToAST(context: DefinitiContext): RootFile = {
    val verifications = ListBuffer[Verification]()
    val classDefinitions = ListBuffer[ClassDefinition]()

    scalaSeq(context.toplevel()).foreach { element =>
      (element.verification(), element.definedType(), element.aliasType()) match {
        case (verification, null, null) =>
          verifications.append(processVerification(verification))
        case (null, definedType, null) =>
          classDefinitions.append(processDefinedType(definedType))
        case (null, null, aliasType) =>
          classDefinitions.append(processAliasType(aliasType))
        case _ =>
          // Because all types were treated before, this exception should throw on new element addition.
          throw new RuntimeException("Unexpected token: " + element)
      }
    }

    RootFile(
      packageName = extractPackageName(context),
      imports = extractImports(context),
      verifications = List(verifications: _*),
      classDefinitions = List(classDefinitions: _*)
    )
  }

  private def processVerification(context: VerificationContext): Verification = {
    Verification(
      name = context.verificationName.getText,
      packageName = NOT_DEFINED,
      message = extractStringContent(context.verificationMessage.getText),
      function = processFunction(context.function()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      getRangeFromContext(context)
    )
  }

  private def processDefinedType(context: DefinedTypeContext): DefinedType = {
    DefinedType(
      name = context.typeName.getText,
      packageName = NOT_DEFINED,
      genericTypes = processGenericTypeListDefinition(context.genericTypeList()),
      attributes = scalaSeq(context.attributeDefinition()).map(processAttributeDefinition),
      verifications = scalaSeq(context.typeVerification()).map(processTypeVerification),
      inherited = scalaSeq(context.inheritance()).map(_.verificationName.getText),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      range = getRangeFromContext(context)
    )
  }

  private def processAttributeDefinition(context: AttributeDefinitionContext): AttributeDefinition = {
    AttributeDefinition(
      name = context.attributeName.getText,
      typeReference = TypeReference(context.attributeType.getText, processGenericTypeList(context.genericTypeList())),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      genericTypes = processGenericTypeList(context.genericTypeList()),
      range = getRangeFromContext(context)
    )
  }

  private def processTypeVerification(context: TypeVerificationContext): TypeVerification = {
    TypeVerification(
      extractStringContent(context.verificationMessage.getText),
      processFunction(context.function()),
      getRangeFromContext(context)
    )
  }

  private def processAliasType(context: AliasTypeContext): AliasType = {
    AliasType(
      name = context.typeName.getText,
      packageName = NOT_DEFINED,
      alias = TypeReference(
        typeName = context.referenceTypeName.getText,
        genericTypes = processGenericTypeList(context.aliasGenericTypes)
      ),
      genericTypes = processGenericTypeListDefinition(context.genericTypes),
      inherited = scalaSeq(context.inheritance()).map(_.verificationName.getText),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      range = getRangeFromContext(context)
    )
  }

  private def processFunction(context: FunctionContext): DefinedFunction = {
    val parameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter)
    implicit val scope = Scope(parametersToVariables(parameters))
    DefinedFunction(
      parameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter),
      body = processChainedExpression(context.chainedExpression()),
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      getRangeFromContext(context)
    )
  }

  private def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = TypeReference(context.parameterType.getText, processGenericTypeList(context.genericTypeList())),
      getRangeFromContext(context)
    )
  }

  private def processChainedExpression(context: ChainedExpressionContext)(implicit scope: Scope): Expression = {
    scalaSeq(context.expression()) match {
      case head :: Nil => processExpression(head)
      case expressions => CombinedExpression(expressions.map(processExpression), getRangeFromContext(context))
    }
  }

  private def processExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
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
    } else if (context.variableExpression != null) {
      processVariableExpression(context)
    } else if (context.conditionExpression != null) {
      processConditionExpression(context)
    } else if (context.lambdaExpression != null) {
      processLambdaExpression(context)
    } else {
      // This exception exists to remind us to implement expression processing when we add one
      // This should never happen in production code.
      throw new RuntimeException(s"Expression ${context.getText} was not processed")
    }
  }

  private def processParenthesisExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    processExpression(context.parenthesis)
  }

  private def processMethodCallExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
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

  private def processAttributeCallExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    AttributeCall(
      expression = processExpression(context.attributeExpression),
      attribute = context.attributeName.getText,
      getRangeFromContext(context)
    )
  }

  private def processNotExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    Not(processExpression(context.notExpression), getRangeFromContext(context))
  }

  private def processLeftRightExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
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

  private def processBooleanExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    context.booleanExpression.getText match {
      case "true" => BooleanValue(value = true, getRangeFromContext(context))
      case _ => BooleanValue(value = false, getRangeFromContext(context))
    }
  }

  private def processNumberExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    NumberValue(BigDecimal(context.numberExpression.getText), getRangeFromContext(context))
  }

  private def processStringExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    QuotedStringValue(extractStringContent(context.stringExpression.getText), getRangeFromContext(context))
  }

  private def processVariableExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    val variableName = context.variableExpression.getText
    scope.variables.find(variable => variable.name == variableName) match {
      case Some(variable) =>
        Variable(
          name = variableName,
          typeReference = variable.typeReference,
          getRangeFromContext(context)
        )
      case None =>
        throw new RuntimeException(s"The variable $variableName was not found")
    }
  }

  private def processConditionExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    Condition(
      condition = processExpression(context.conditionExpression),
      onTrue = processChainedExpression(context.conditionIfBody),
      onFalse = Option(context.conditionElseBody).map(processChainedExpression),
      getRangeFromContext(context)
    )
  }

  private def processLambdaExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    val lambdaParameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter)
    val outerVariableNotShadowed = scope.variables.filter(v => !lambdaParameters.exists(_.name == v.name))
    val innerVariables = parametersToVariables(lambdaParameters) ++ outerVariableNotShadowed
    val innerScope = Scope(innerVariables)
    LambdaExpression(
      parameterList = lambdaParameters,
      expression = processExpression(context.lambdaExpression)(innerScope),
      range = getRangeFromContext(context)
    )
  }

  private def processGenericTypeList(context: GenericTypeListContext): Seq[TypeReference] = {
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

  private def processGenericTypeListDefinition(context: GenericTypeListContext) = {
    Option(context)
      .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
      .getOrElse(Seq())
  }

  private def extractPackageName(context: DefinitiContext): String = {
    Option(context.packageName())
      .map(packageNameContext => dottedIdentifierToIdentifier(packageNameContext.dottedIdentifier()))
      .getOrElse("")
  }

  private def extractImports(context: DefinitiContext): Map[String, String] = {
    scalaSeq(context.imports())
      .view
      .map(importContext => dottedIdentifierToIdentifier(importContext.dottedIdentifier()))
      .map(fullName => StringUtils.lastPart(fullName, '.') -> fullName)
      .toMap
  }

  private def dottedIdentifierToIdentifier(context: DottedIdentifierContext): String = {
    scalaSeq(context.IDENTIFIER()).map(_.getText).mkString(".")
  }
}
