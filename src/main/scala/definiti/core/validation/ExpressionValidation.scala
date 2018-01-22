package definiti.core.validation

import definiti.core._
import definiti.core.ast._

private[core] trait ExpressionValidation {
  self: ASTValidation =>

  private[definiti] def validateExpression(expression: Expression): Validation = {
    import definiti.core.ast.LogicalOperator._
    expression match {
      case _: BooleanValue => Valid
      case _: NumberValue => Valid
      case _: QuotedStringValue => Valid
      case _: Reference => Valid
      case methodCall: MethodCall =>
        validateMethodCall(methodCall)
      case attributeCall: AttributeCall =>
        validateAttributeCall(attributeCall)
      case CombinedExpression(parts, _, _) =>
        Validation.join(parts.map(validateExpression))
      case condition: Condition =>
        validateCondition(condition)
      case LogicalExpression(Or | And, left, right, _, _) =>
        validateBooleanExpression(left, right)
      case LogicalExpression(Equal | NotEqual | Lower | Upper | LowerOrEqual | UpperOrEqual, left, right, _, _) =>
        validateExpressions(left, right)
      case CalculatorExpression(_, left, right, _, _) =>
        validateExpressions(left, right)
      case lambdaExpression: LambdaExpression =>
        // Expected only in method call, processed in validateMethodCall or validateFunctionCall
        Invalid("Unexpected lambda reference", lambdaExpression.location)
      case functionCallExpression: FunctionCall =>
        validateFunctionCall(functionCallExpression)
      case not: Not =>
        validateNotExpression(not)
    }
  }

  private def validateNotExpression(not: Not): Validation = {
    validateDeepBooleanExpression(not.inner)
  }

  private def validateCondition(condition: Condition): Validation = {
    Validation.join(
      validateDeepBooleanExpression(condition.condition),
      validateExpression(condition.onTrue),
      condition.onFalse.map(validateExpression).getOrElse(Valid)
    )
  }

  private def validateAttributeCall(attributeCall: AttributeCall): Validation = {
    validateExpression(attributeCall.expression)
  }

  def validateMethodCall(methodCall: MethodCall): Validation = {
    getReturnType(methodCall.expression)
      .flatMap { classDefinition =>
        getMethodDefinition(methodCall).flatMap { methodDefinition =>
          if (methodDefinition.parameters.length == methodCall.parameters.length) {
            Validation.join(
              methodDefinition.parameters.zip(methodCall.parameters)
                .map(p => validateParameter(p._1, p._2, classDefinition, methodDefinition))
            )
          } else {
            Invalid("Invalid number of arguments", methodCall.location)
          }
        }
      }
      .toValidation
  }

  def getMethodDefinition(methodCall: MethodCall): Validated[MethodDefinition] = {
    getReturnTypeName(methodCall.expression).flatMap { typeName =>
      library.methodsMap.get(s"${typeName}.${methodCall.method}") match {
        case Some(method) => ValidValue(method)
        case None => Invalid(s"Unknown method ${typeName}.${methodCall.method}", methodCall.location)
      }
    }
  }

  def validateParameter(definedParameter: ParameterDefinition, callParameter: Expression, classDefinition: ClassDefinition, methodDefinition: MethodDefinition): Validation = {
    (definedParameter.typeReference, callParameter) match {
      case (lambdaReference: LambdaReference, lambdaExpression: LambdaExpression) =>
        validateLambdaExpressionAndReference(lambdaExpression, lambdaReference, classDefinition, methodDefinition)
      case (lambdaReference: LambdaReference, expression) =>
        expression.returnType match {
          case NamedFunctionReference(functionName) =>
            library.namedFunctionsMap.get(functionName) match {
              case Some(namedFunction) =>
                validateNamedFunctionAndReference(namedFunction, lambdaReference, classDefinition, methodDefinition, callParameter.location)
              case None =>
                Invalid(s"Undefined function: ${functionName}", callParameter.location)
            }
          case _ =>
            Invalid("Expected lambda expression or function reference", callParameter.location)
        }
      case (_, _: LambdaExpression) =>
        Invalid("Unexpected lambda expression", callParameter.location)
      case (typeReference, expression) =>
        validateReturnTypeExpression(expression, typeReference, classDefinition, methodDefinition)
    }
  }

  def validateReturnTypeExpression(expression: Expression, expectedReturnType: AbstractTypeReference, classDefinition: ClassDefinition, methodDefinition: MethodDefinition): Validation = {
    getReturnType(expression)
      .flatMap { classDefinition =>
        expectedReturnType match {
          case TypeReference(typeName, _) if classDefinition.name == typeName =>
            Valid
          case typeReference: TypeReference if isGeneric(typeReference.typeName, classDefinition, methodDefinition) =>
            Valid
          case TypeReference(typeName, _) =>
            Invalid(s"Unexpected return type $typeName, expected ${classDefinition.name}", expression.location)
          case _ =>
            Invalid("Unexpected lambda reference", expression.location)
        }
      }
      .toValidation
  }

  def validateLambdaExpressionAndReference(lambdaExpression: LambdaExpression, lambdaReference: LambdaReference, classDefinition: ClassDefinition, methodDefinition: MethodDefinition): Validation = {
    val expressionParameters = lambdaExpression.parameterList
    val referenceParameters = lambdaReference.inputTypes
    if (expressionParameters.length == referenceParameters.length) {
      Validation.join(expressionParameters.zip(referenceParameters) map { case (expressionParameter, referenceParameter) =>
        expressionParameter.typeReference match {
          case TypeReference(typeName, _) if referenceParameter.typeName == typeName =>
            Valid
          case _: TypeReference if isGeneric(referenceParameter.typeName, classDefinition, methodDefinition) =>
            Valid
          case TypeReference(typeName, _) =>
            Invalid(s"Unexpected type $typeName, expected ${referenceParameter.typeName}", expressionParameter.location)
          case _ =>
            Invalid("Unexpected lambda reference", expressionParameter.location)
        }
      })
    } else {
      Invalid(s"Invalid number of arguments. Expected ${referenceParameters.length}, got ${expressionParameters.length}", lambdaExpression.location)
    }
  }

  def validateNamedFunctionAndReference(namedFunction: NamedFunction, lambdaReference: LambdaReference, classDefinition: ClassDefinition, methodDefinition: MethodDefinition, location: Location): Validation = {
    val expressionParameters = namedFunction.parameters
    val referenceParameters = lambdaReference.inputTypes
    if (expressionParameters.length == referenceParameters.length) {
      Validation.join(expressionParameters.zip(referenceParameters) map { case (expressionParameter, referenceParameter) =>
        expressionParameter.typeReference match {
          case TypeReference(typeName, _) if referenceParameter.typeName == typeName =>
            Valid
          case _: TypeReference if isGeneric(referenceParameter.typeName, classDefinition, methodDefinition) =>
            Valid
          case TypeReference(typeName, _) =>
            Invalid(s"Unexpected type $typeName, expected ${referenceParameter.typeName}", expressionParameter.location)
          case _ =>
            Invalid("Unexpected lambda reference", expressionParameter.location)
        }
      })
    } else {
      Invalid(s"Invalid number of arguments. Expected ${referenceParameters.length}, got ${expressionParameters.length}", location)
    }
  }

  def validateFunctionCall(functionCall: FunctionCall): Validation = {
    getNamedFunction(functionCall.name, functionCall.location)
      .flatMap { namedFunction =>
        if (namedFunction.parameters.length == functionCall.parameters.length) {
          Validation.join(namedFunction.parameters.zip(functionCall.parameters).map { case (definedParameter, callParameter) =>
            (definedParameter.typeReference, callParameter) match {
              case (lambdaReference: LambdaReference, lambdaExpression: LambdaExpression) =>
                validateLambdaExpressionAndReferenceForFunctionCall(lambdaExpression, lambdaReference, namedFunction)
              case (_: LambdaReference, _) =>
                Invalid("Expected lambda expression", callParameter.location)
              case (_, _: LambdaExpression) =>
                Invalid("Unexpected lambda expression", callParameter.location)
              case (typeReference, expression) =>
                validateReturnTypeExpressionForFunctionCall(expression, typeReference, namedFunction)
            }
          })
        } else {
          Invalid("Invalid number of arguments", functionCall.location)
        }
      }
      .toValidation
  }

  private def getNamedFunction(functionName: String, location: Location): Validated[NamedFunction] = {
    library.namedFunctionsMap.get(functionName) match {
      case Some(namedFunction) => ValidValue(namedFunction)
      case None => Invalid(s"Unknown function ${functionName}", location)
    }
  }

  def validateReturnTypeExpressionForFunctionCall(expression: Expression, expectedReturnType: AbstractTypeReference, namedFunction: NamedFunction): Validation = {
    getReturnType(expression)
      .flatMap {
        classDefinition =>
          expectedReturnType match {
            case TypeReference(typeName, _) if classDefinition.name == typeName =>
              Valid
            case typeReference: TypeReference if isGeneric(typeReference.typeName, namedFunction) =>
              Valid
            case TypeReference(typeName, _) =>
              Invalid(s"Unexpected return type $typeName, expected ${
                classDefinition.name
              }", expression.location)
            case _ =>
              Invalid("Unexpected lambda reference", expression.location)
          }
      }
      .toValidation
  }

  def validateLambdaExpressionAndReferenceForFunctionCall(lambdaExpression: LambdaExpression, lambdaReference: LambdaReference, namedFunction: NamedFunction): Validation = {
    val expressionParameters = lambdaExpression.parameterList
    val referenceParameters = lambdaReference.inputTypes
    if (expressionParameters.length == referenceParameters.length) {
      Validation.join(expressionParameters.zip(referenceParameters) map { case (expressionParameter, referenceParameter) =>
        expressionParameter.typeReference match {
          case TypeReference(typeName, _) if referenceParameter.typeName == typeName =>
            Valid
          case _: TypeReference if isGeneric(referenceParameter.typeName, namedFunction) =>
            Valid
          case TypeReference(typeName, _) =>
            Invalid(s"Unexpected type $typeName, expected ${referenceParameter.typeName}", expressionParameter.location)
          case _ =>
            Invalid("Unexpected lambda reference", expressionParameter.location)
        }
      })
    } else {
      Invalid(s"Invalid number of arguments. Expected ${referenceParameters.length}, got ${expressionParameters.length}", lambdaExpression.location)
    }
  }

  def isGeneric(typeName: String, classDefinition: ClassDefinition, methodDefinition: MethodDefinition): Boolean = {
    val isGenericTypeFromClass = classDefinition.genericTypes.contains(typeName)
    val isGenericTypeFromMethod = methodDefinition.genericTypes.contains(typeName)
    isGenericTypeFromClass || isGenericTypeFromMethod
  }

  def isGeneric(typeName: String, namedFunction: NamedFunction): Boolean = {
    namedFunction.genericTypes.contains(typeName)
  }

  def validateExpressions(expressions: Expression*): Validation = {
    Validation.join(expressions.map(validateExpression))
  }

  def validateBooleanExpression(left: Expression, right: Expression): Validation = {
    validateExpressions(left, right).verifyingAlso {
      Validation.join(
        validateDeepBooleanExpression(left),
        validateDeepBooleanExpression(right)
      )
    }
  }

  def validateDeepBooleanExpression(expression: Expression): Validation = {
    Validation.join(
      validateExpression(expression),
      validateBooleanExpression(expression)
    )
  }
}
