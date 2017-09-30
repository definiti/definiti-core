package definiti.core.validation

import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._

private[core] class ASTValidation(configuration: Configuration) extends CommonValidation {
  def validate(root: Root)(implicit context: Context): Validation = {
    val verificationValidations = root.files.flatMap(_.verifications).map(validateVerification)

    val classDefinitionValidations = root.files.flatMap(_.classDefinitions).map {
      case aliasType: AliasType => validateAliasType(aliasType)
      case definedType: DefinedType => validateDefinedType(definedType)
      case _ => Valid
    }

    val namedFunctionValidations = root.files.flatMap(_.namedFunctions).map(validateNamedFunction)

    Validation.join(verificationValidations ++ classDefinitionValidations ++ namedFunctionValidations)
  }

  def validateVerification(verification: Verification)(implicit context: Context): Validation = {
    val functionContext = DefinedFunctionContext(
      outerContext = context,
      currentFunction = verification.function
    )
    validateExpression(verification.function.body)(functionContext).verifyingAlso {
      validateBooleanExpression(verification.function.body)(functionContext)
    }
  }

  def validateAliasType(aliasType: AliasType)(implicit context: Context): Validation = {
    verifyTypeReference(aliasType).verifyingAlso {
      Validation.join(aliasType.inherited.map { verification =>
        context.findVerification(verification) match {
          case Some(_) => Valid
          case None => Invalid("Undefined verification: " + verification.verificationName, aliasType.range)
        }
      })
    }
  }

  private[definiti] def validateDefinedType(definedType: DefinedType)(implicit context: Context): Validation = {
    val inheritedValidations = definedType.inherited.map { verification =>
      context.findVerification(verification) match {
        case Some(_) => Valid
        case None => Invalid("Undefined verification: " + verification.verificationName, definedType.range)
      }
    }
    val attributeValidations = definedType.attributes.map(validateAttributeDefinition)
    val verificationValidations = definedType.verifications.map { verification =>
      val verificationContext = DefinedFunctionContext(
        outerContext = context,
        currentFunction = verification.function
      )
      validateBooleanExpression(verification.function.body)(verificationContext)
    }
    Validation.join(inheritedValidations ++ attributeValidations ++ verificationValidations)
  }

  def validateAttributeDefinition(attribute: AttributeDefinition)(implicit context: Context): Validation = {
    val typeReferenceValidation = context.findType(attribute.typeReference.typeName) match {
      case Some(_) => Valid
      case None => Invalid("Undefined type: " + attribute.typeReference.readableString, attribute.range)
    }
    val verificationsValidation = attribute.verifications.map { verification =>
      if (context.isVerificationAvailable(verification)) {
        Valid
      } else {
        Invalid("Undefined verification: " + verification.verificationName, attribute.range)
      }
    }
    Validation.join(typeReferenceValidation +: verificationsValidation)
  }

  def validateNamedFunction(namedFunction: NamedFunction)(implicit context: Context): Validation = {
    val methodContext = NamedFunctionReferenceContext(
      outerContext = context,
      currentFunction = namedFunction
    )
    validateExpression(namedFunction.body)(methodContext)
  }

  private[definiti] def validateExpression(expression: Expression)(implicit context: Context): Validation = {
    import LogicalOperator._
    expression match {
      case BooleanValue(_, _) => Valid
      case NumberValue(_, _) => Valid
      case QuotedStringValue(_, _) => Valid
      case Reference(name, range) =>
        context.findReference(name) match {
          case Some(_) => Valid
          case None => Invalid("Unknown reference: " + name, range)
        }
      case methodCall: MethodCall =>
        validateMethodCall(methodCall)
      case attributeCall: AttributeCall =>
        validateAttributeCall(attributeCall)
      case CombinedExpression(parts, _) =>
        Validation.join(parts.map(validateExpression))
      case condition: Condition =>
        validateCondition(condition)
      case LogicalExpression(Or | And, left, right, _) =>
        validateBooleanExpression(left, right)
      case LogicalExpression(Equal | NotEqual | Lower | Upper | LowerOrEqual | UpperOrEqual, left, right, _) =>
        validateExpressions(left, right)
      case CalculatorExpression(_, left, right, _) =>
        validateExpressions(left, right)
      case lambdaExpression: LambdaExpression =>
        // Expected only in method call, processed in validateMethodCall or validateFunctionCall
        Invalid("Unexpected lambda reference", lambdaExpression.range)
      case functionCallExpression: FunctionCall =>
        validateFunctionCall(functionCallExpression)
      case not: Not =>
        validateNotExpression(not)
    }
  }

  private def validateNotExpression(not: Not)(implicit context: Context): Validation = {
    validateBooleanExpression(not.inner)
  }

  private def validateCondition(condition: Condition)(implicit context: Context): Validation = {
    Validation.join(
      validateBooleanExpression(condition.condition),
      validateExpression(condition.onTrue),
      condition.onFalse.map(validateExpression).getOrElse(Valid)
    )
  }

  private def validateAttributeCall(attributeCall: AttributeCall)(implicit context: Context): Validation = {
    val inner = attributeCall.expression
    val attribute = attributeCall.attribute
    val range = attributeCall.range
    validateExpression(inner).verifyingAlso {
      expectingClassDefinition(inner) { classDefinition =>
        ASTHelper.getAttributeOpt(classDefinition, attribute) match {
          case Some(_) => Valid
          case None => Invalid(s"Unknown attribute ${classDefinition.name}.$attribute", range)
        }
      }
    }
  }

  def validateMethodCall(methodCall: MethodCall)(implicit context: Context): Validation = {
    expectingClassDefinition(methodCall.expression) { classDefinition =>
      ASTHelper.getMethodOpt(classDefinition, methodCall.method) match {
        case Some(methodDefinition) =>
          if (methodDefinition.parameters.length == methodCall.parameters.length) {
            Validation.join(
              methodDefinition.parameters.zip(methodCall.parameters)
                .map(p => validateParameter(p._1, p._2, classDefinition, methodDefinition))
            )
          } else {
            Invalid("Invalid number of arguments", methodCall.range)
          }
        case None => Invalid(s"Unknown method ${classDefinition.name}.${methodCall.method}", methodCall.range)
      }
    }
  }

  def validateParameter(definedParameter: ParameterDefinition, callParameter: Expression, classDefinition: ClassDefinition, methodDefinition: MethodDefinition)(implicit context: Context): Validation = {
    (definedParameter.typeReference, callParameter) match {
      case (lambdaReference: LambdaReference, lambdaExpression: LambdaExpression) =>
        validateLambdaExpressionAndReference(lambdaExpression, lambdaReference, classDefinition, methodDefinition)
      case (lambdaReference: LambdaReference, expression) =>
        ASTHelper.getReturnTypeOptOfExpression(expression) match {
          case Some(NamedFunctionReference(namedFunction)) =>
            validateNamedFunctionAndReference(namedFunction, lambdaReference, classDefinition, methodDefinition, callParameter.range)
          case _ =>
            Invalid("Expected lambda expression or function reference", callParameter.range)
        }
      case (_, _: LambdaExpression) =>
        Invalid("Unexpected lambda expression", callParameter.range)
      case (typeReference, expression) =>
        validateReturnTypeExpression(expression, typeReference, classDefinition, methodDefinition)
    }
  }

  def validateReturnTypeExpression(expression: Expression, expectedReturnType: AbstractTypeReference, classDefinition: ClassDefinition, methodDefinition: MethodDefinition)(implicit context: Context): Validation = {
    expectingClassDefinition(expression) {
      classDefinition =>
        expectedReturnType match {
          case TypeReference(typeName, _) if classDefinition.name == typeName =>
            Valid
          case typeReference: TypeReference if isGeneric(typeReference.typeName, classDefinition, methodDefinition) =>
            Valid
          case TypeReference(typeName, _) =>
            Invalid(s"Unexpected return type $typeName, expected ${
              classDefinition.name
            }", expression.range)
          case _ =>
            Invalid("Unexpected lambda reference", expression.range)
        }
    }
  }

  def validateLambdaExpressionAndReference(lambdaExpression: LambdaExpression, lambdaReference: LambdaReference, classDefinition: ClassDefinition, methodDefinition: MethodDefinition)(implicit context: Context): Validation = {
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
            Invalid(s"Unexpected type $typeName, expected ${referenceParameter.typeName}", expressionParameter.range)
          case _ =>
            Invalid("Unexpected lambda reference", expressionParameter.range)
        }
      })
    } else {
      Invalid(s"Invalid number of arguments. Expected ${referenceParameters.length}, got ${expressionParameters.length}", lambdaExpression.range)
    }
  }

  def validateNamedFunctionAndReference(namedFunction: NamedFunction, lambdaReference: LambdaReference, classDefinition: ClassDefinition, methodDefinition: MethodDefinition, range: Range)(implicit context: Context): Validation = {
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
            Invalid(s"Unexpected type $typeName, expected ${referenceParameter.typeName}", expressionParameter.range)
          case _ =>
            Invalid("Unexpected lambda reference", expressionParameter.range)
        }
      })
    } else {
      Invalid(s"Invalid number of arguments. Expected ${referenceParameters.length}, got ${expressionParameters.length}", range)
    }
  }

  def validateFunctionCall(functionCall: FunctionCall)(implicit context: Context): Validation = {
    context.findFunction(functionCall.name) match {
      case Some(function) =>
        if (function.parameters.length == functionCall.parameters.length) {
          Validation.join(function.parameters.zip(functionCall.parameters).map { case (definedParameter, callParameter) =>
            (definedParameter.typeReference, callParameter) match {
              case (lambdaReference: LambdaReference, lambdaExpression: LambdaExpression) =>
                validateLambdaExpressionAndReferenceForFunctionCall(lambdaExpression, lambdaReference, function)
              case (_: LambdaReference, _) =>
                Invalid("Expected lambda expression", callParameter.range)
              case (_, _: LambdaExpression) =>
                Invalid("Unexpected lambda expression", callParameter.range)
              case (typeReference, expression) =>
                validateReturnTypeExpressionForFunctionCall(expression, typeReference, function)
            }
          })
        } else {
          Invalid("Invalid number of arguments", functionCall.range)
        }
      case None =>
        Invalid(s"Unknown function ${functionCall.name}", functionCall.range)
    }
  }

  def validateReturnTypeExpressionForFunctionCall(expression: Expression, expectedReturnType: AbstractTypeReference, namedFunction: NamedFunction)(implicit context: Context): Validation = {
    expectingClassDefinition(expression) {
      classDefinition =>
      expectedReturnType match {
        case TypeReference(typeName, _) if classDefinition.name == typeName =>
          Valid
        case typeReference: TypeReference if isGeneric(typeReference.typeName, namedFunction) =>
          Valid
        case TypeReference(typeName, _) =>
          Invalid(s"Unexpected return type $typeName, expected ${
            classDefinition.name
          }", expression.range)
        case _ =>
          Invalid("Unexpected lambda reference", expression.range)
      }
    }
  }

  def validateLambdaExpressionAndReferenceForFunctionCall(lambdaExpression: LambdaExpression, lambdaReference: LambdaReference, namedFunction: NamedFunction)(implicit context: Context): Validation = {
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
            Invalid(s"Unexpected type $typeName, expected ${referenceParameter.typeName}", expressionParameter.range)
          case _ =>
            Invalid("Unexpected lambda reference", expressionParameter.range)
        }
      })
    } else {
      Invalid(s"Invalid number of arguments. Expected ${referenceParameters.length}, got ${expressionParameters.length}", lambdaExpression.range)
    }
  }

  def isGeneric(typeName: String, classDefinition: ClassDefinition, methodDefinition: MethodDefinition)(implicit context: Context): Boolean = {
    val isGenericTypeFromClass = classDefinition.genericTypes.contains(typeName)
    val isGenericTypeFromMethod = methodDefinition.genericTypes.contains(typeName)
    isGenericTypeFromClass || isGenericTypeFromMethod
  }

  def isGeneric(typeName: String, namedFunction: NamedFunction)(implicit context: Context): Boolean = {
    namedFunction.genericTypes.contains(typeName)
  }

  def validateExpressions(expressions: Expression*)(implicit context: Context): Validation = {
    Validation.join(expressions.map(validateExpression))
  }

  def validateBooleanExpression(left: Expression, right: Expression)(implicit context: Context): Validation = {
    validateExpressions(left, right).verifyingAlso {
      Validation.join(
        validateBooleanExpression(left),
        validateBooleanExpression(right)
      )
    }
  }

  def verifyTypeReference(aliasType: AliasType)(implicit context: Context): Validation = {
    if (context.isTypeAvailable(aliasType.alias.typeName)) {
      Valid
    } else {
      Invalid("Undefined type: " + aliasType.alias.readableString, aliasType.range)
    }
  }

  def validateTypeReferenceOfExpression(expression: Expression)(implicit context: Context): Validation = {
    expression match {
      case _: LogicalExpression => Valid
      case _: CalculatorExpression => Valid
      case _: Not => Valid
      case _: BooleanValue => Valid
      case NumberValue(_, _) => Valid
      case QuotedStringValue(_, _) => Valid
      case Reference(name, range) =>
        if (context.isReferencesAvailable(name)) {
          Valid
        } else {
          Invalid(s"Unknown reference $name", range)
        }
      case MethodCall(innerExpression, method, _, _, range) =>
        validateTypeReferenceOfExpression(innerExpression).verifyingAlso {
          expectingClassDefinition(innerExpression) {
            classDefinition =>
              ASTHelper.getMethodOpt(classDefinition, method) match {
                case Some(_) =>
                  // We do not need to validate the method return type because it should be done in upper level
                  Valid
                case None =>
                  Invalid(s"Unknown method $method on type ${classDefinition.name}", range)
              }
          }
        }
      case AttributeCall(innerExpression, attribute, range) =>
        validateTypeReferenceOfExpression(innerExpression).verifyingAlso {
          expectingClassDefinition(innerExpression) {
            classDefinition =>
              ASTHelper.getAttributeOpt(classDefinition, attribute) match {
                case Some(_) =>
                  // We do not need to validate the attribute type because it should be done in upper level
                  Valid
                case None =>
                  Invalid(s"Unknown attribute $attribute on type ${classDefinition.name}", range)
              }
          }
        }
      case CombinedExpression(parts, _) =>
        Validation.join(parts.map(validateTypeReferenceOfExpression))
      case Condition(condition, onTrue, onFalseOpt, _) =>
        Validation.join(
          validateTypeReferenceOfExpression(condition).verifyingAlso {
            validateBooleanExpression(condition)
          },
          validateTypeReferenceOfExpression(onTrue),
          onFalseOpt.map(validateTypeReferenceOfExpression).getOrElse(Valid)
        )
      case _ =>
        throw new RuntimeException("Unexpected expression: " + expression)
    }
  }

  def validateBooleanExpression(expression: Expression)(implicit context: Context): Validation = {
    validateExpression(expression) verifyingAlso {
      validateBooleanReference(ASTHelper.getReturnTypeOfExpression(expression), expression.range)
    }
  }

  def validateBooleanReference(elementReference: ElementReference, range: Range)(implicit context: Context): Validation = {
    expectingClassDefinition(elementReference, range) {
      classDefinition =>
        if (classDefinition.name == BOOLEAN) {
          Valid
        } else {
          Invalid("Expected boolean expression, got: class " + classDefinition.name, range)
        }
    }
  }

  def expectingClassDefinition(expression: Expression)(validator: ClassDefinition => Validation)(implicit context: Context): Validation = {
    validateExpression(expression) verifyingAlso {
      expectingClassDefinition(ASTHelper.getReturnTypeOfExpression(expression), expression.range)(validator)
    }
  }

  def expectingClassDefinition(elementReference: ElementReference, range: Range)(validator: ClassDefinition => Validation)(implicit context: Context): Validation = {
    elementReference match {
      case ClassReference(classDefinition, _) =>
        validator(classDefinition)
      case NamedFunctionReference(namedFunction) =>
        Invalid("Expected class, got: function " + namedFunction.name, range)
    }
  }

  def validateContexts[A](context: ExtendedContext[A])(implicit outerContext: Context): Validation = {
    configuration.contexts
      .find(_.contextName == context.name)
      .map { contextPlugin =>
        // asInstanceOf because it should be the exact same plugin than for parsing.
        contextPlugin.asInstanceOf[ContextPlugin[A]]
          .validate(context.content)
      }
      .getOrElse(Valid)
  }
}
