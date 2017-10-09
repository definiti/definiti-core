package definiti.core.typing

import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._

private[core] class ExpressionTyping(context: Context) {
  import ExpressionTyping._

  def addTypesIntoExpression(expression: PureExpression): Validated[Expression] = {
    expression match {
      case PureLogicalExpression(operator, left, right, location) => addTypeIntoLogicalExpression(operator, left, right, location)
      case PureCalculatorExpression(operator, left, right, location) => addTypeIntoCalculatorExpression(operator, left, right, location)
      case not: PureNot => addTypesIntoNotExpression(not)

      case booleanValue: PureBooleanValue => ValidValue(BooleanValue(booleanValue.value, boolean, booleanValue.location))
      case numberValue: PureNumberValue => ValidValue(NumberValue(numberValue.value, number, numberValue.location))
      case quotedStringValue: PureQuotedStringValue => ValidValue(QuotedStringValue(quotedStringValue.value, string, quotedStringValue.location))

      case reference: PureReference => addTypesIntoReference(reference)

      case methodCall: PureMethodCall => addTypeIntoMethodCall(methodCall)
      case attributeCall: PureAttributeCall => addTypeIntoAttributeCall(attributeCall)

      case combinedExpression: PureCombinedExpression => addTypeIntoCombinedExpression(combinedExpression)

      case condition: PureCondition => addTypeIntoCondition(condition)

      case lambdaExpression: PureLambdaExpression => addTypeIntoLambdaExpression(lambdaExpression)
      case functionCall: PureFunctionCall => addTypeIntoFunctionCall(functionCall)
    }
  }

  def addTypeIntoLogicalExpression(operator: LogicalOperator.Value, left: PureExpression, right: PureExpression, location: Location): Validated[Expression] = {
    Validated.both(addTypesIntoExpression(left), addTypesIntoExpression(right))
      .map { case (typedLeft, typedRight) =>
        LogicalExpression(operator, typedLeft, typedRight, boolean, location)
      }
  }

  def addTypeIntoCalculatorExpression(operator: CalculatorOperator.Value, left: PureExpression, right: PureExpression, location: Location): Validated[Expression] = {
    Validated.both(addTypesIntoExpression(left), addTypesIntoExpression(right))
      .map { case (typedLeft, typedRight) =>
        CalculatorExpression(operator, typedLeft, typedRight, number, location)
      }
  }

  def addTypesIntoNotExpression(not: PureNot): Validated[Expression] = {
    addTypesIntoExpression(not.inner).map { expression =>
      Not(expression, boolean, not.location)
    }
  }

  def addTypesIntoReference(reference: PureReference): Validated[Expression] = {
    context.findTypeReference(reference.name) match {
      case Some(typeReference) =>
        ValidValue(Reference(
          name = reference.name,
          returnType = typeReference,
          location = reference.location
        ))
      case None => Invalid("Unknown reference: " + reference.name, reference.location)
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
            classReferenceFromTypeReference(
              typeReference, expression.location,
              typeReference => s"Class ${typeReference.typeName} not found when trying to determine the type of the expression"
            ).flatMap { classReference =>
                getMethodOpt(classReference.classDefinition, methodCall.method)(context) match {
                  case Some(methodDefinition) =>
                    ValidValue(MethodCall(
                      expression = expression,
                      method = methodCall.method,
                      parameters = parameters,
                      generics = methodCall.generics,
                      returnType = methodDefinition.returnType,
                      location = methodCall.location
                    ))
                  case None => Invalid(s"Unknown method ${typeReference.typeName}.${methodCall.method}", methodCall.location)
                }
              }
          case _: LambdaReference => Invalid("Expected type, got lambda", expression.location)
        }
      }
  }

  def classReferenceFromTypeReference(reference: TypeReference, location: Location, messageBuilder: TypeReference => String): Validated[ClassReference] = {
    classReferenceFromTypeReference(reference, location, Some(messageBuilder))
  }

  def classReferenceFromTypeReference(typeReference: TypeReference, location: Location, messageBuilder: Option[TypeReference => String] = None): Validated[ClassReference] = {
    context.findType(typeReference.typeName) match {
      case Some(classDefinition) =>
        Validated
          .squash(typeReference.genericTypes.map(classReferenceFromTypeReference(_, location, messageBuilder)))
          .map(ClassReference(classDefinition, _))
      case None =>
        val message = messageBuilder
          .map(f => f(typeReference))
          .getOrElse(s"Class ${typeReference.typeName} not found")
        Invalid(message, location)
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
          classReferenceFromTypeReference(
            typeReference, expression.location,
            typeReference => s"Class ${typeReference.typeName} not found when trying to determine the type of the expression"
          ).flatMap { classReference =>
              getAttributeOpt(classReference.classDefinition, attributeCall.attribute)(context) match {
                case Some(attributeDefinition) =>
                  ValidValue(AttributeCall(
                    expression = expression,
                    attribute = attributeCall.attribute,
                    returnType = attributeDefinition.typeReference,
                    location = attributeCall.location
                  ))
                case None => Invalid(s"Unknown attribute ${typeReference.typeName}.${attributeCall.attribute}", attributeCall.location)
              }
            }
        case _: LambdaReference => Invalid("Expected type, got lambda", expression.location)
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
      .map(parts => CombinedExpression(parts, parts.last.returnType, combinedExpression.location))
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
          location = condition.location
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
          location = lambdaExpression.location
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
            location = functionCall.location
          )
        }
      case None =>
        Invalid(s"Unknown function ${functionCall.name}", functionCall.location)
    }
  }
}

object ExpressionTyping {
  type LeftRightExpressionConstructor = (Expression, Expression, TypeReference, Range) => Expression

  val boolean = TypeReference("Boolean", Seq.empty)
  val number = TypeReference("Number", Seq.empty)
  val string = TypeReference("String", Seq.empty)
}