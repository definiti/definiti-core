package definiti.core.typing

import definiti.common.ast._
import definiti.common.validation.{Invalid, Valid, Validated}
import definiti.core._
import definiti.core.ast.pure._

private[core] class ExpressionTyping(context: Context) {
  import ExpressionTyping._

  def addTypesIntoExpression(expression: PureExpression): Validated[Expression] = {
    expression match {
      case PureLogicalExpression(operator, left, right, location) => addTypeIntoLogicalExpression(operator, left, right, location)
      case PureCalculatorExpression(operator, left, right, location) => addTypeIntoCalculatorExpression(operator, left, right, location)
      case not: PureNot => addTypesIntoNotExpression(not)

      case atomicExpression: PureAtomicExpression => addTypeIntoAtomicExpression(atomicExpression)

      case methodCall: PureMethodCall => addTypeIntoMethodCall(methodCall)
      case attributeCall: PureAttributeCall => addTypeIntoAttributeCall(attributeCall)

      case combinedExpression: PureCombinedExpression => addTypeIntoCombinedExpression(combinedExpression)

      case condition: PureCondition => addTypeIntoCondition(condition)

      case lambdaExpression: PureLambdaExpression => addTypeIntoLambdaExpression(lambdaExpression)
      case functionCall: PureFunctionCall => addTypeIntoFunctionCall(functionCall)

      case okValue: PureOkValue => addTypesIntoOkValue(okValue)
      case koValue: PureKoValue => addTypesIntoKoValue(koValue)
    }
  }

  def addTypeIntoAtomicExpression(expression: PureAtomicExpression): Validated[AtomicExpression] = {
    expression match {
      case booleanValue: PureBooleanValue => Valid(BooleanValue(booleanValue.value, boolean, booleanValue.location))
      case numberValue: PureNumberValue => Valid(NumberValue(numberValue.value, number, numberValue.location))
      case quotedStringValue: PureQuotedStringValue => Valid(QuotedStringValue(quotedStringValue.value, string, quotedStringValue.location))
      case reference: PureReference => addTypesIntoReference(reference)
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

  def addTypesIntoReference(reference: PureReference): Validated[Reference] = {
    context.findTypeReference(reference.name) match {
      case Some(typeReference) =>
        Valid(Reference(
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
                  val typedMethodDefinition = considerTypeIntoMethodDefinition(methodDefinition, classReference)
                  Valid(MethodCall(
                    expression = expression,
                    method = methodCall.method,
                    parameters = parameters,
                    generics = methodCall.generics,
                    returnType = returnTypeOfMethod(typedMethodDefinition, parameters),
                    location = methodCall.location
                  ))
                case None => Invalid(s"Unknown method ${typeReference.typeName}.${methodCall.method}", methodCall.location)
              }
            }
          case _: LambdaReference => Invalid("Expected type, got lambda", expression.location)
        }
      }
  }

  private def considerTypeIntoMethodDefinition(methodDefinition: MethodDefinition, classReference: ClassReference): MethodDefinition = {
    def adaptTypeReference(typeReference: TypeReference): TypeReference = {
      if (classReference.classDefinition.genericTypes.contains(typeReference.typeName)) {
        classReferenceToTypeReference(classReference.genericTypes(classReference.classDefinition.genericTypes.indexOf(typeReference.typeName)))
      } else {
        TypeReference(
          typeName = typeReference.typeName,
          genericTypes = typeReference.genericTypes.map(adaptTypeReference)
        )
      }
    }

    methodDefinition.copy(
      parameters = methodDefinition.parameters.map { parameter =>
        parameter.copy(
          typeReference = parameter.typeReference match {
            case typeReference: TypeReference => adaptTypeReference(typeReference)
            case LambdaReference(inputTypes, outputType) => LambdaReference(inputTypes.map(adaptTypeReference), adaptTypeReference(outputType))
            case namedFunctionReference: NamedFunctionReference => namedFunctionReference
          }
        )
      },
      returnType = adaptTypeReference(methodDefinition.returnType)
    )
  }

  private def classReferenceToTypeReference(classReference: ClassReference): TypeReference = {
    TypeReference(
      typeName = classReference.classDefinition.canonicalName,
      genericTypes = classReference.genericTypes.map(classReferenceToTypeReference)
    )
  }

  private def returnTypeOfMethod(methodDefinition: MethodDefinition, parameters: Seq[Expression]): TypeReference = {
    def process(typeReference: TypeReference): TypeReference = {
      if (methodDefinition.genericTypes.contains(typeReference.typeName)) {
        methodDefinition.parameters.zip(parameters)
          .flatMap { case (parameterDefinition, parameterCall) =>
            (parameterDefinition.typeReference, parameterCall, parameterCall.returnType) match {
              case (TypeReference(typeName, _), _: Expression, returnType: TypeReference) if typeName == typeReference.typeName =>
                Some(returnType)
              case (LambdaReference(_, outputType), _: LambdaExpression, returnType: TypeReference) if outputType.typeName == typeReference.typeName =>
                Some(returnType)
              case _ =>
                None
            }
          }
          .headOption
          .getOrElse(typeReference)
      } else {
        TypeReference(
          typeName = typeReference.typeName,
          genericTypes = typeReference.genericTypes.map(process)
        )
      }
    }

    process(methodDefinition.returnType)
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
                Valid(AttributeCall(
                  expression = expression,
                  attribute = attributeCall.attribute,
                  returnType = typeDeclarationToTypeReference(attributeDefinition.typeDeclaration),
                  location = attributeCall.location
                ))
              case None => Invalid(s"Unknown attribute ${typeReference.typeName}.${attributeCall.attribute}", attributeCall.location)
            }
          }
        case _: LambdaReference => Invalid("Expected type, got lambda", expression.location)
      }
    }
  }

  private def typeDeclarationToTypeReference(typeDeclaration: PureTypeDeclaration): TypeReference = {
    TypeReference(
      typeName = typeDeclaration.typeName,
      genericTypes = typeDeclaration.genericTypes.map(typeDeclarationToTypeReference)
    )
  }

  def getAttributeOpt(classDefinition: PureClassDefinition, attribute: String)(implicit context: Context): Option[PureAttributeDefinition] = {
    classDefinition match {
      case nativeClassDefinition: PureNativeClassDefinition =>
        nativeClassDefinition.attributes.find(_.name == attribute)
      case aliasType: PureAliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getAttributeOpt(_, attribute))
      case definedType: PureDefinedType =>
        definedType.attributes.find(_.name == attribute)
      case enum: PureEnum =>
        enum.cases
          .find(_.name == attribute)
          .map { enumCase =>
            // Actually this is quite a little workaround because it is not really an attribute with all their feature.
            // But it avoid rewriting lot of code outside just to consider enum and their cases.
            PureAttributeDefinition(
              name = enumCase.name,
              typeDeclaration = PureTypeDeclaration(
                enum.canonicalName,
                genericTypes = Seq.empty,
                parameters = Seq.empty,
                location = enum.location
              ),
              comment = enumCase.comment,
              verifications = Seq.empty,
              location = enumCase.location
            )
          }
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

  private def addTypesIntoOkValue(okValue: PureOkValue): Validated[OkValue] = {
    Valid {
      OkValue(
        returnType = okko,
        location = okValue.location
      )
    }
  }

  private def addTypesIntoKoValue(koValue: PureKoValue): Validated[KoValue] = {
    val validatedTypedParameters = Validated.squash(koValue.parameters.map(addTypesIntoExpression))
    validatedTypedParameters.map { parameters =>
      KoValue(
        parameters = parameters,
        returnType = okko,
        location = koValue.location
      )
    }
  }
}

object ExpressionTyping {
  type LeftRightExpressionConstructor = (Expression, Expression, TypeReference, Range) => Expression

  val boolean = TypeReference("Boolean", Seq.empty)
  val number = TypeReference("Number", Seq.empty)
  val string = TypeReference("String", Seq.empty)
  val okko = TypeReference("OkKo", Seq.empty)
}