package definiti.core.parser

import definiti.core._

private[core] sealed trait Validation {
  def join(other: Validation): Validation

  def verifyingAlso(nextValidation: => Validation): Validation
}

private[core] object Validation {
  def join(validations: Seq[Validation]): Validation = {
    validations.foldLeft(Valid.asInstanceOf[Validation]) { (acc, validation) => acc.join(validation) }
  }

  def join(validations: Validation*)(implicit dummyImplicit: DummyImplicit): Validation = {
    join(validations)
  }
}

private[core] case object Valid extends Validation {
  override def join(other: Validation): Validation = other

  override def verifyingAlso(nextValidation: => Validation): Validation = nextValidation
}

private[core] case class Invalid(errors: Seq[Error]) extends Validation {
  def join(other: Validation): Validation = other match {
    case Valid => this
    case Invalid(otherErrors) => Invalid(errors ++ otherErrors)
  }

  override def verifyingAlso(nextValidation: => Validation): Validation = this
}

private[core] object Invalid {
  def apply(message: String, range: Range): Invalid = new Invalid(Seq(Error(message, range)))
}

private[core] case class Error(message: String, range: Range) {
  def prettyPrint: String = {
    s"""Error at ${range.prettyPrint}: $message"""
  }
}

private[core] object ASTValidation {
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
    validateExpression(verification.function.body).verifyingAlso {
      val functionReturnType = ASTHelper.getReturnTypeOfExpression(verification.function.body)
      if (functionReturnType.classDefinition.name == "Boolean") {
        Valid
      } else {
        Invalid("The function in verification must be a boolean expression, got: " + functionReturnType.classDefinition.name, verification.function.body.range)
      }
    }
  }

  def validateAliasType(aliasType: AliasType)(implicit context: Context): Validation = {
    verifyTypeReference(aliasType).verifyingAlso {
      Validation.join(aliasType.inherited.map { verification =>
        context.findVerification(verification) match {
          case Some(_) => Valid
          case None => Invalid("Undefined verification: " + verification, aliasType.range)
        }
      })
    }
  }

  private[definiti] def validateDefinedType(definedType: DefinedType)(implicit context: Context): Validation = {
    val inheritedValidations = definedType.inherited.map { verification =>
      context.findVerification(verification) match {
        case Some(_) => Valid
        case None => Invalid("Undefined verification: " + verification, definedType.range)
      }
    }
    val attributeValidations = definedType.attributes.map(validateAttributeDefinition)
    val verificationValidations = definedType.verifications.map { verification =>
      validateExpression(verification.function.body).verifyingAlso {
        ASTHelper.getReturnTypeOptOfExpression(verification.function.body) match {
          case Some(functionReturnType) if functionReturnType.classDefinition.name == "Boolean" =>
            Valid
          case Some(functionReturnType) =>
            Invalid("The function in verification must be a Boolean, got: " + functionReturnType.classDefinition.name, verification.function.body.range)
          case None =>
            Invalid("Could not find the type of expression.", verification.function.body.range)
        }
      }
    }
    Validation.join(inheritedValidations ++ attributeValidations ++ verificationValidations)
  }

  def validateAttributeDefinition(attribute: AttributeDefinition)(implicit context: Context): Validation = {
    val typeReferenceValidation = context.findType(attribute.typeReference.typeName) match {
      case Some(_) => Valid
      case None => Invalid("Undefined type: " + attribute.typeReference, attribute.range)
    }
    val verificationsValidation = attribute.verifications.map { verification =>
      if (context.isVerificationAvailable(verification)) {
        Valid
      } else {
        Invalid("Undefined verification: " + verification, attribute.range)
      }
    }
    Validation.join(typeReferenceValidation +: verificationsValidation)
  }

  def validateNamedFunction(namedFunction: NamedFunction)(implicit context: Context): Validation = {
    validateExpression(namedFunction.function.body)
  }

  private[definiti] def validateExpression(expression: Expression)(implicit context: Context): Validation = expression match {
    case BooleanValue(_, _) => Valid
    case NumberValue(_, _) => Valid
    case QuotedStringValue(_, _) => Valid
    case Variable(_, typeReference, range) =>
      context.findType(typeReference.typeName) match {
        case Some(_) => Valid
        case None => Invalid("Unknown type: " + typeReference, range)
      }
    case methodCall: MethodCall =>
      validateMethodCall(methodCall)
    case AttributeCall(inner, attribute, range) =>
      validateExpression(inner).verifyingAlso {
        val innerReturnType = ASTHelper.getReturnTypeOfExpression(inner)
        ASTHelper.getAttributeOpt(innerReturnType.classDefinition, attribute) match {
          case Some(_) => Valid
          case None => Invalid(s"Unknown attribute ${innerReturnType.classDefinition.name}.$attribute", range)
        }
      }
    case CombinedExpression(parts, _) =>
      Validation.join(parts.map(validateExpression))
    case Condition(condition, onTrue, onFalse, _) =>
      validateExpression(condition)
      validateExpression(onTrue)
      onFalse.foreach(validateExpression)
      val conditionReturnType = ASTHelper.getReturnTypeOfExpression(condition)
      if (conditionReturnType.classDefinition.name == "Boolean") {
        Valid
      } else {
        Invalid("The condition must be a boolean expression, got: " + conditionReturnType.classDefinition.name, condition.range)
      }
    case Or(left, right, _) =>
      validateBooleanExpression(left, right)
    case And(left, right, _) =>
      validateBooleanExpression(left, right)
    case Equal(left, right, _) =>
      validateExpressions(left, right)
    case NotEqual(left, right, _) =>
      validateExpressions(left, right)
    case Lower(left, right, _) =>
      validateExpressions(left, right)
    case Upper(left, right, _) =>
      validateExpressions(left, right)
    case LowerOrEqual(left, right, _) =>
      validateExpressions(left, right)
    case UpperOrEqual(left, right, _) =>
      validateExpressions(left, right)
    case Plus(left, right, _) =>
      validateExpressions(left, right)
    case Minus(left, right, _) =>
      validateExpressions(left, right)
    case Modulo(left, right, _) =>
      validateExpressions(left, right)
    case Time(left, right, _) =>
      validateExpressions(left, right)
    case Divide(left, right, _) =>
      validateExpressions(left, right)
    case lambdaExpression: LambdaExpression =>
      // Expected only in method call, processed in validateMethodCall or validateFunctionCall
      Invalid("Unexpected lambda reference", lambdaExpression.range)
    case functionCallExpression: FunctionCall =>
      validateFunctionCall(functionCallExpression)
    case Not(inner, _) =>
      validateExpression(inner).verifyingAlso {
        val innerReturnType = ASTHelper.getReturnTypeOfExpression(inner)
        if (innerReturnType.classDefinition.name == "Boolean") {
          Valid
        } else {
          Invalid("The expression must be a boolean expression, got: " + innerReturnType.classDefinition.name, inner.range)
        }
      }
  }

  def validateMethodCall(methodCall: MethodCall)(implicit context: Context): Validation = {
    validateExpression(methodCall.expression).verifyingAlso {
      val innerReturnType = ASTHelper.getReturnTypeOfExpression(methodCall.expression)
      ASTHelper.getMethodOpt(innerReturnType.classDefinition, methodCall.method) match {
        case Some(methodDefinition) =>
          if (methodDefinition.parameters.length == methodCall.parameters.length) {
            Validation.join(methodDefinition.parameters.zip(methodCall.parameters).map { case (definedParameter, callParameter) =>
              (definedParameter.typeReference, callParameter) match {
                case (lambdaReference: LambdaReference, lambdaExpression: LambdaExpression) =>
                  validateLambdaExpressionAndReference(lambdaExpression, lambdaReference, innerReturnType.classDefinition, methodDefinition)
                case (_: LambdaReference, _) =>
                  Invalid("Expected lambda expression", callParameter.range)
                case (_, _: LambdaExpression) =>
                  Invalid("Unexpected lambda expression", callParameter.range)
                case (typeReference, expression) =>
                  validateReturnTypeExpression(expression, typeReference, innerReturnType.classDefinition, methodDefinition)
              }
            })
          } else {
            Invalid("Invalid number of arguments", methodCall.range)
          }
        case None => Invalid(s"Unknown method ${innerReturnType.classDefinition.name}.${methodCall.method}", methodCall.range)
      }
    }
  }

  def validateReturnTypeExpression(expression: Expression, expectedReturnType: AbstractTypeReference, classDefinition: ClassDefinition, methodDefinition: MethodDefinition)(implicit context: Context): Validation = {
    val expressionReturnTypeOpt = ASTHelper.getReturnTypeOptOfExpression(expression)
    expressionReturnTypeOpt.map { expressionReturnType =>
      expectedReturnType match {
        case TypeReference(typeName, _) if expressionReturnType.classDefinition.name == typeName =>
          Valid
        case typeReference: TypeReference if isGeneric(typeReference.typeName, classDefinition, methodDefinition) =>
          Valid
        case TypeReference(typeName, _) =>
          Invalid(s"Unexpected return type $typeName, expected ${expressionReturnType.classDefinition.name}", expression.range)
        case _ =>
          Invalid("Unexpected lambda reference", expression.range)
      }
    } getOrElse Invalid("Can not determine return type of expression", expression.range)
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
    val expressionReturnTypeOpt = ASTHelper.getReturnTypeOptOfExpression(expression)
    expressionReturnTypeOpt.map { expressionReturnType =>
      expectedReturnType match {
        case TypeReference(typeName, _) if expressionReturnType.classDefinition.name == typeName =>
          Valid
        case typeReference: TypeReference if isGeneric(typeReference.typeName, namedFunction) =>
          Valid
        case TypeReference(typeName, _) =>
          Invalid(s"Unexpected return type $typeName, expected ${expressionReturnType.classDefinition.name}", expression.range)
        case _ =>
          Invalid("Unexpected lambda reference", expression.range)
      }
    } getOrElse Invalid("Can not determine return type of expression", expression.range)
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
      lazy val leftReturnType = ASTHelper.getReturnTypeOfExpression(left)
      lazy val rightReturnType = ASTHelper.getReturnTypeOfExpression(right)
      if (leftReturnType.classDefinition.name != "Boolean") {
        Invalid("The left part of logical expression must be a boolean expression, got: " + leftReturnType.classDefinition.name, left.range)
      } else if (rightReturnType.classDefinition.name != "Boolean") {
        Invalid("The right part of logical expression must be a boolean expression, got: " + rightReturnType.classDefinition.name, right.range)
      } else {
        Valid
      }
    }
  }

  def verifyTypeReference(aliasType: AliasType)(implicit context: Context): Validation = {
    if (context.isTypeAvailable(aliasType.alias.typeName)) {
      Valid
    } else {
      Invalid("Undefined type: " + aliasType.alias, aliasType.range)
    }
  }

  def validateTypeReferencesOfMethod(methodDefinition: MethodDefinition)(implicit context: Context): Validation = {
    methodDefinition match {
      case _: NativeMethodDefinition =>
        Valid
      case definedMethodDefinition: DefinedMethodDefinition =>
        validateTypeReferenceOfExpression(definedMethodDefinition.function.body)
    }
  }

  def validateTypeReferenceOfExpression(expression: Expression)(implicit context: Context): Validation = {
    expression match {
      case _: LogicalExpression => Valid
      case _: CalculatorExpression => Valid
      case NumberValue(_, _) => Valid
      case QuotedStringValue(_, _) => Valid
      case Variable(name, typeReference, range) =>
        if (context.isTypeAvailable(typeReference.typeName)) {
          Valid
        } else {
          Invalid(s"Unknown type $typeReference for variable $name", range)
        }
      case MethodCall(innerExpression, method, _, _, range) =>
        validateTypeReferenceOfExpression(innerExpression).verifyingAlso {
          val innerType = ASTHelper.getReturnTypeOfExpression(innerExpression)
          ASTHelper.getMethodOpt(innerType.classDefinition, method) match {
            case Some(_) =>
              // We do not need to validate the method return type because it should be done in upper level
              Valid
            case None =>
              Invalid(s"Unknown method $method on type ${innerType.classDefinition.name}", range)
          }
        }
      case AttributeCall(innerExpression, attribute, range) =>
        validateTypeReferenceOfExpression(innerExpression).verifyingAlso {
          val innerType = ASTHelper.getReturnTypeOfExpression(innerExpression)
          ASTHelper.getAttributeOpt(innerType.classDefinition, attribute) match {
            case Some(_) =>
              // We do not need to validate the attribute type because it should be done in upper level
              Valid
            case None =>
              Invalid(s"Unknown attribute $attribute on type ${innerType.classDefinition.name}", range)
          }
        }
      case CombinedExpression(parts, _) =>
        Validation.join(parts.map(validateTypeReferenceOfExpression))
      case Condition(condition, onTrue, onFalseOpt, _) =>
        Validation.join(
          validateTypeReferenceOfExpression(condition).verifyingAlso {
            if (ASTHelper.getReturnTypeOfExpression(condition).classDefinition.name == "Boolean") {
              Valid
            } else {
              Invalid("The condition must be a boolean expression", condition.range)
            }
          },
          validateTypeReferenceOfExpression(onTrue),
          onFalseOpt.map(validateTypeReferenceOfExpression).getOrElse(Valid)
        )
    }
  }
}
