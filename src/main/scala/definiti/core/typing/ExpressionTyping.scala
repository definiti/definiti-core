package definiti.core.typing

import definiti.core._
import definiti.core.ast.pure._
import definiti.core.ast.{Range, pure, typed}

class ExpressionTyping(context: Context) {
  import ExpressionTyping._

  def addTypesIntoExpression(expression: pure.Expression): Validated[typed.Expression] = {
    expression match {
      case LogicalExpression(operator, left, right, range) => addTypeIntoLogicalExpression(operator, left, right, range)
      case CalculatorExpression(operator, left, right, range) => addTypeIntoCalculatorExpression(operator, left, right, range)
      case not: pure.Not => addTypesIntoNotExpression(not)

      case booleanValue: pure.BooleanValue => ValidValue(typed.BooleanValue(booleanValue.value, boolean, booleanValue.range))
      case numberValue: pure.NumberValue => ValidValue(typed.NumberValue(numberValue.value, number, numberValue.range))
      case quotedStringValue: pure.QuotedStringValue => ValidValue(typed.QuotedStringValue(quotedStringValue.value, string, quotedStringValue.range))

      case reference: pure.Reference => addTypesIntoReference(reference)

      case methodCall: pure.MethodCall => addTypeIntoMethodCall(methodCall)
      case attributeCall: pure.AttributeCall => addTypeIntoAttributeCall(attributeCall)

      case combinedExpression: pure.CombinedExpression => addTypeIntoCombinedExpression(combinedExpression)

      case condition: pure.Condition => addTypeIntoCondition(condition)

      case lambdaExpression: pure.LambdaExpression => addTypeIntoLambdaExpression(lambdaExpression)
      case functionCall: pure.FunctionCall => addTypeIntoFunctionCall(functionCall)
    }
  }

  def addTypeIntoLogicalExpression(operator: LogicalOperator.Value, left: pure.Expression, right: pure.Expression, range: Range): Validated[typed.Expression] = {
    Validated.both(addTypesIntoExpression(left), addTypesIntoExpression(right))
      .map { case (typedLeft, typedRight) =>
        typed.LogicalExpression(operator, typedLeft, typedRight, boolean, range)
      }
  }

  def addTypeIntoCalculatorExpression(operator: CalculatorOperator.Value, left: pure.Expression, right: pure.Expression, range: Range): Validated[typed.Expression] = {
    Validated.both(addTypesIntoExpression(left), addTypesIntoExpression(right))
      .map { case (typedLeft, typedRight) =>
        typed.CalculatorExpression(operator, typedLeft, typedRight, number, range)
      }
  }

  def addTypesIntoNotExpression(not: pure.Not): Validated[typed.Expression] = {
    addTypesIntoExpression(not.inner).map { expression =>
      typed.Not(expression, boolean, not.range)
    }
  }

  def addTypesIntoReference(reference: pure.Reference): Validated[typed.Expression] = {
    context.findTypeReference(reference.name) match {
      case Some(typeReference) =>
        ValidValue(typed.Reference(
          name = reference.name,
          returnType = typeReference,
          range = reference.range
        ))
      case None => Invalid("Unknown reference: " + reference.name, reference.range)
    }
  }

  def addTypeIntoMethodCall(methodCall: pure.MethodCall): Validated[typed.MethodCall] = {
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
                    ValidValue(typed.MethodCall(
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

  def getMethodOpt(classDefinition: ClassDefinition, method: String)(implicit context: Context): Option[MethodDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.methods.find(_.name == method)
      case aliasType: AliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getMethodOpt(_, method))
      case definedType: DefinedType =>
        definedType.methods.find(_.name == method)
    }
  }

  def addTypeIntoAttributeCall(attributeCall: pure.AttributeCall): Validated[typed.AttributeCall] = {
    val validatedTypedExpression = addTypesIntoExpression(attributeCall.expression)
    validatedTypedExpression.flatMap { expression =>
      expression.returnType match {
        case typeReference: TypeReference =>
          classReferenceFromTypeReference(typeReference, expression.range)
            .flatMap { classReference =>
              getAttributeOpt(classReference.classDefinition, attributeCall.attribute)(context) match {
                case Some(attributeDefinition) =>
                  ValidValue(typed.AttributeCall(
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

  def getAttributeOpt(classDefinition: ClassDefinition, attribute: String)(implicit context: Context): Option[AttributeDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.attributes.find(_.name == attribute)
      case aliasType: AliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getAttributeOpt(_, attribute))
      case definedType: DefinedType =>
        definedType.attributes.find(_.name == attribute)
    }
  }

  def addTypeIntoCombinedExpression(combinedExpression: pure.CombinedExpression): Validated[typed.CombinedExpression] = {
    Validated
      .squash(combinedExpression.parts.map(addTypesIntoExpression))
      .map(parts => typed.CombinedExpression(parts, parts.last.returnType, combinedExpression.range))
  }

  def addTypeIntoCondition(condition: pure.Condition): Validated[typed.Condition] = {
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
        typed.Condition(
          condition = conditionExpression,
          onTrue = onTrue,
          onFalse = onFalse,
          returnType = returnType,
          range = condition.range
        )
      }
  }

  def addTypeIntoLambdaExpression(lambdaExpression: pure.LambdaExpression): Validated[typed.LambdaExpression] = {
    val lambdaContext = LambdaContext(context, lambdaExpression)
    val lambdaExpressionTyping = new ExpressionTyping(lambdaContext)
    lambdaExpressionTyping.addTypesIntoExpression(lambdaExpression.expression)
      .map { expression =>
        typed.LambdaExpression(
          parameterList = lambdaExpression.parameterList,
          expression = expression,
          returnType = expression.returnType,
          range = lambdaExpression.range
        )
      }
  }

  def addTypeIntoFunctionCall(functionCall: pure.FunctionCall): Validated[typed.FunctionCall] = {
    context.findFunction(functionCall.name) match {
      case Some(namedFunction) =>
        val validatedTypedParameters = Validated.squash(functionCall.parameters.map(addTypesIntoExpression))
        validatedTypedParameters.map { parameters =>
          typed.FunctionCall(
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
  type LeftRightExpressionConstructor = (typed.Expression, typed.Expression, pure.TypeReference, Range) => typed.Expression

  val boolean = pure.TypeReference("Boolean", Seq.empty)
  val number = pure.TypeReference("Number", Seq.empty)
  val string = pure.TypeReference("String", Seq.empty)
}