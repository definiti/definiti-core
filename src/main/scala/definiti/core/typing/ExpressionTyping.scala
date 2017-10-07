package definiti.core.typing

import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._

private[core] class ExpressionTyping(context: Context) {
  import ExpressionTyping._

  def addTypesIntoExpression(expression: PureExpression): Validated[Expression] = {
    expression match {
      case PureLogicalExpression(operator, left, right, range) => addTypeIntoLogicalExpression(operator, left, right, range)
      case PureCalculatorExpression(operator, left, right, range) => addTypeIntoCalculatorExpression(operator, left, right, range)
      case not: PureNot => addTypesIntoNotExpression(not)

      case booleanValue: PureBooleanValue => ValidValue(BooleanValue(booleanValue.value, boolean, booleanValue.range))
      case numberValue: PureNumberValue => ValidValue(NumberValue(numberValue.value, number, numberValue.range))
      case quotedStringValue: PureQuotedStringValue => ValidValue(QuotedStringValue(quotedStringValue.value, string, quotedStringValue.range))

      case reference: PureReference => addTypesIntoReference(reference)

      case methodCall: PureMethodCall => addTypeIntoMethodCall(methodCall)
      case attributeCall: PureAttributeCall => addTypeIntoAttributeCall(attributeCall)

      case combinedExpression: PureCombinedExpression => addTypeIntoCombinedExpression(combinedExpression)

      case condition: PureCondition => addTypeIntoCondition(condition)

      case lambdaExpression: PureLambdaExpression => addTypeIntoLambdaExpression(lambdaExpression)
      case functionCall: PureFunctionCall => addTypeIntoFunctionCall(functionCall)
    }
  }

  def addTypeIntoLogicalExpression(operator: LogicalOperator.Value, left: PureExpression, right: PureExpression, range: Range): Validated[Expression] = {
    Validated.both(addTypesIntoExpression(left), addTypesIntoExpression(right))
      .map { case (typedLeft, typedRight) =>
        LogicalExpression(operator, typedLeft, typedRight, boolean, range)
      }
  }

  def addTypeIntoCalculatorExpression(operator: CalculatorOperator.Value, left: PureExpression, right: PureExpression, range: Range): Validated[Expression] = {
    Validated.both(addTypesIntoExpression(left), addTypesIntoExpression(right))
      .map { case (typedLeft, typedRight) =>
        CalculatorExpression(operator, typedLeft, typedRight, number, range)
      }
  }

  def addTypesIntoNotExpression(not: PureNot): Validated[Expression] = {
    addTypesIntoExpression(not.inner).map { expression =>
      Not(expression, boolean, not.range)
    }
  }

  def addTypesIntoReference(reference: PureReference): Validated[Expression] = {
    context.findTypeReference(reference.name) match {
      case Some(typeReference) =>
        ValidValue(Reference(
          name = reference.name,
          returnType = typeReference,
          range = reference.range
        ))
      case None => Invalid("Unknown reference: " + reference.name, reference.range)
    }
  }

  def addTypeIntoMethodCall(methodCall: PureMethodCall): Validated[MethodCall] = {
    val validatedTypedExpression = addTypesIntoExpression(methodCall.expression)
    val validatedTypedParameters = Validated.squash(methodCall.parameters.map(addTypesIntoExpression))
    Validated
      .both(validatedTypedExpression, validatedTypedParameters)
      .flatMap { case (expression, parameters) =>
        expression.returnType match {
          case typeReference: TypeReference =>
            classReferenceFromTypeReference(typeReference, expression.range)
              .flatMap { classReference =>
                getMethodOpt(classReference.classDefinition, methodCall.method)(context) match {
                  case Some(methodDefinition) =>
                    ValidValue(MethodCall(
                      expression = expression,
                      method = methodCall.method,
                      parameters = parameters,
                      generics = methodCall.generics,
                      returnType = methodDefinition.returnType,
                      range = methodCall.range
                    ))
                  case None => Invalid(s"Unknown method ${typeReference.typeName}.${methodCall.method}", methodCall.range)
                }
              }
          case _: LambdaReference => Invalid("Expected type, got lambda", expression.range)
        }
      }
  }

  def classReferenceFromTypeReference(typeReference: TypeReference, range: Range): Validated[ClassReference] = {
    context.findType(typeReference.typeName) match {
      case Some(classDefinition) =>
        Validated
          .squash(typeReference.genericTypes.map(classReferenceFromTypeReference(_, range)))
          .map(ClassReference(classDefinition, _))
      case None =>
        Invalid(s"Class ${typeReference.typeName} not found", range)
    }
  }

  def getMethodOpt(classDefinition: PureClassDefinition, method: String)(implicit context: Context): Option[MethodDefinition] = {
    classDefinition match {
      case nativeClassDefinition: PureNativeClassDefinition =>
        nativeClassDefinition.methods.find(_.name == method)
      case aliasType: PureAliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getMethodOpt(_, method))
      case definedType: PureDefinedType =>
        definedType.methods.find(_.name == method)
    }
  }

  def addTypeIntoAttributeCall(attributeCall: PureAttributeCall): Validated[AttributeCall] = {
    val validatedTypedExpression = addTypesIntoExpression(attributeCall.expression)
    validatedTypedExpression.flatMap { expression =>
      expression.returnType match {
        case typeReference: TypeReference =>
          classReferenceFromTypeReference(typeReference, expression.range)
            .flatMap { classReference =>
              getAttributeOpt(classReference.classDefinition, attributeCall.attribute)(context) match {
                case Some(attributeDefinition) =>
                  ValidValue(AttributeCall(
                    expression = expression,
                    attribute = attributeCall.attribute,
                    returnType = attributeDefinition.typeReference,
                    range = attributeDefinition.range
                  ))
                case None => Invalid(s"Unknown attribute ${typeReference.typeName}.${attributeCall.attribute}", attributeCall.range)
              }
            }
        case _: LambdaReference => Invalid("Expected type, got lambda", expression.range)
      }
    }
  }

  def getAttributeOpt(classDefinition: PureClassDefinition, attribute: String)(implicit context: Context): Option[AttributeDefinition] = {
    classDefinition match {
      case nativeClassDefinition: PureNativeClassDefinition =>
        nativeClassDefinition.attributes.find(_.name == attribute)
      case aliasType: PureAliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getAttributeOpt(_, attribute))
      case definedType: PureDefinedType =>
        definedType.attributes.find(_.name == attribute)
    }
  }

  def addTypeIntoCombinedExpression(combinedExpression: PureCombinedExpression): Validated[CombinedExpression] = {
    Validated
      .squash(combinedExpression.parts.map(addTypesIntoExpression))
      .map(parts => CombinedExpression(parts, parts.last.returnType, combinedExpression.range))
  }

  def addTypeIntoCondition(condition: PureCondition): Validated[Condition] = {
    val validatedTypedCondition = addTypesIntoExpression(condition.condition)
    val validatedTypedOnTrue = addTypesIntoExpression(condition.onTrue)
    val validatedTypeOnFalse = Validated.reverseOption(condition.onFalse.map(addTypesIntoExpression))
    Validated
      .both(validatedTypedCondition, validatedTypedOnTrue, validatedTypeOnFalse)
      .map { case (conditionExpression, onTrue, onFalse) =>
        val returnType = if (onFalse.exists(_.returnType == onTrue.returnType)) {
          onTrue.returnType
        } else {
          TypeReference("unit", Seq.empty)
        }
        Condition(
          condition = conditionExpression,
          onTrue = onTrue,
          onFalse = onFalse,
          returnType = returnType,
          range = condition.range
        )
      }
  }

  def addTypeIntoLambdaExpression(lambdaExpression: PureLambdaExpression): Validated[LambdaExpression] = {
    val lambdaContext = LambdaContext(context, lambdaExpression)
    val lambdaExpressionTyping = new ExpressionTyping(lambdaContext)
    lambdaExpressionTyping.addTypesIntoExpression(lambdaExpression.expression)
      .map { expression =>
        LambdaExpression(
          parameterList = lambdaExpression.parameterList,
          expression = expression,
          returnType = expression.returnType,
          range = lambdaExpression.range
        )
      }
  }

  def addTypeIntoFunctionCall(functionCall: PureFunctionCall): Validated[FunctionCall] = {
    context.findFunction(functionCall.name) match {
      case Some(namedFunction) =>
        val validatedTypedParameters = Validated.squash(functionCall.parameters.map(addTypesIntoExpression))
        validatedTypedParameters.map { parameters =>
          FunctionCall(
            name = functionCall.name,
            parameters = parameters,
            generics = functionCall.generics,
            returnType = namedFunction.returnType,
            range = functionCall.range
          )
        }
      case None =>
        Invalid(s"Unknown function ${functionCall.name}", functionCall.range)
    }
  }
}

object ExpressionTyping {
  type LeftRightExpressionConstructor = (Expression, Expression, TypeReference, Range) => Expression

  val boolean = TypeReference("Boolean", Seq.empty)
  val number = TypeReference("Number", Seq.empty)
  val string = TypeReference("String", Seq.empty)
}