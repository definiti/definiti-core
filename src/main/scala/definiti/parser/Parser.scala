package definiti.parser

import definiti._
import definiti.parser.antlr.DefinitiParser._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

case class Scope(variables: Seq[Variable])

object Scope {
  val empty = Scope(Seq())
}

object Parser {
  def definitiContextToAST(context: DefinitiContext): Root = {
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

    Root(verifications, classDefinitions)
  }

  private def processVerification(context: VerificationContext): Verification = {
    Verification(
      name = context.verificationName.getText,
      message = extractStringContent(context.verificationMessage.getText),
      function = processFunction(context.function()),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processDefinedType(context: DefinedTypeContext): DefinedType = {
    DefinedType(
      name = context.typeName.getText,
      attributes = scalaSeq(context.attributeDefinition()).map(processAttributeDefinition),
      verifications = scalaSeq(context.typeVerification()).map(processTypeVerification),
      inherited = scalaSeq(context.inheritance()).map(_.verificationName.getText),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processAttributeDefinition(context: AttributeDefinitionContext): AttributeDefinition = {
    AttributeDefinition(
      name = context.attributeName.getText,
      typeReference = context.attributeType.getText,
      Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processTypeVerification(context: TypeVerificationContext): TypeVerification = {
    TypeVerification(
      extractStringContent(context.verificationMessage.getText),
      processFunction(context.function())
    )
  }

  private def processAliasType(context: AliasTypeContext): AliasType = {
    AliasType(
      name = context.typeName.getText,
      alias = context.referenceTypeName.getText,
      inherited = scalaSeq(context.inheritance()).map(_.verificationName.getText),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processFunction(context: FunctionContext): DefinedFunction = {
    val parameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter)
    implicit val scope = Scope(parameters.map(parameter => Variable(parameter.name, parameter.typeReference)))
    DefinedFunction(
      parameters = scalaSeq(context.parameterListDefinition().parameterDefinition()).map(processParameter),
      body = processChainedExpression(context.chainedExpression())
    )
  }

  private def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = context.parameterType.getText
    )
  }

  private def processChainedExpression(context: ChainedExpressionContext)(implicit scope: Scope): Expression = {
    scalaSeq(context.expression()) match {
      case head :: Nil => processExpression(head)
      case expressions => CombinedExpression(expressions.map(processExpression))
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
      } getOrElse Seq.empty
    )
  }

  private def processAttributeCallExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    AttributeCall(
      expression = processExpression(context.attributeExpression),
      attribute = context.attributeName.getText
    )
  }

  private def processNotExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    Not(processExpression(context.notExpression))
  }

  private def processLeftRightExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    val left = processExpression(context.leftExpression)
    val right = processExpression(context.rightExpression)
    context.operator.getText match {
      case "*" => Time(left, right)
      case "/" => Divide(left, right)
      case "%" => Modulo(left, right)
      case "+" => Plus(left, right)
      case "-" => Minus(left, right)
      case "==" => Equal(left, right)
      case "!=" => NotEqual(left, right)
      case "<" => Lower(left, right)
      case "<=" => LowerOrEqual(left, right)
      case ">" => Upper(left, right)
      case ">=" => UpperOrEqual(left, right)
      case "&&" => And(left, right)
      case "||" => Or(left, right)
    }
  }

  private def processBooleanExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    context.booleanExpression.getText match {
      case "true" => BooleanValue(true)
      case _ => BooleanValue(false)
    }
  }

  private def processNumberExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    NumberValue(BigDecimal(context.numberExpression.getText))
  }

  private def processStringExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    QuotedStringValue(extractStringContent(context.stringExpression.getText))
  }

  private def processVariableExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    val variableName = context.variableExpression.getText
    scope.variables.find(variable => variable.name == variableName) match {
      case Some(variable) =>
        Variable(
          name = variableName,
          typeReference = variable.typeReference
        )
      case None =>
        throw new RuntimeException(s"The variable $variableName was not found")
    }
  }

  private def processConditionExpression(context: ExpressionContext)(implicit scope: Scope): Expression = {
    Condition(
      condition = processExpression(context.conditionExpression),
      onTrue = processChainedExpression(context.conditionIfBody),
      onFalse = Option(context.conditionElseBody).map(processChainedExpression)
    )
  }

  // Helper used to force the use of immutable seq
  private def scalaSeq[A](list: java.util.List[A]): Seq[A] = {
    list.asScala.toList
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

  private def extractDocComment(string: String): String = {
    var temporaryResult = string
    if (temporaryResult.startsWith("/**")) {
      temporaryResult = temporaryResult.substring(3)
    }
    if (temporaryResult.endsWith("*/")) {
      temporaryResult = temporaryResult.substring(0, temporaryResult.length - 2)
    }
    temporaryResult
  }
}
