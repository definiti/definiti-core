package definiti.core.typing

import definiti.common.ast._
import definiti.common.validation.{Invalid, Valid, Validated}
import definiti.core._

private[core] class ExpressionTyping(context: Context) {
  import ExpressionTyping._

  def addTypesIntoExpression(expression: Expression): Validated[Expression] = {
    expression match {
      case logicalExpression: LogicalExpression => addTypeIntoLogicalExpression(logicalExpression)
      case calculatorExpression: CalculatorExpression => addTypeIntoCalculatorExpression(calculatorExpression)
      case not: Not => addTypesIntoNotExpression(not)

      case atomicExpression: AtomicExpression => addTypeIntoAtomicExpression(atomicExpression)

      case methodCall: MethodCall => addTypeIntoMethodCall(methodCall)
      case attributeCall: AttributeCall => addTypeIntoAttributeCall(attributeCall)

      case combinedExpression: CombinedExpression => addTypeIntoCombinedExpression(combinedExpression)

      case condition: Condition => addTypeIntoCondition(condition)

      case lambdaExpression: LambdaExpression => addTypeIntoLambdaExpression(lambdaExpression)
      case functionCall: FunctionCall => addTypeIntoFunctionCall(functionCall)

      case okValue: OkValue => addTypesIntoOkValue(okValue)
      case koValue: KoValue => addTypesIntoKoValue(koValue)
    }
  }

  def addTypeIntoAtomicExpression(expression: AtomicExpression): Validated[AtomicExpression] = {
    expression match {
      case booleanValue: BooleanValue => Valid(booleanValue.copy(returnType = boolean))
      case numberValue: NumberValue => Valid(numberValue.copy(returnType = number))
      case quotedStringValue: QuotedStringValue => Valid(quotedStringValue.copy(returnType = string))
      case reference: Reference => addTypesIntoReference(reference)
    }
  }

  def addTypeIntoLogicalExpression(logicalExpression: LogicalExpression): Validated[Expression] = {
    Validated.both(addTypesIntoExpression(logicalExpression.left), addTypesIntoExpression(logicalExpression.right))
      .map { case (typedLeft, typedRight) =>
        logicalExpression.copy(
          left = typedLeft,
          right = typedRight,
          returnType = boolean
        )
      }
  }

  def addTypeIntoCalculatorExpression(calculatorExpression: CalculatorExpression): Validated[Expression] = {
    Validated.both(addTypesIntoExpression(calculatorExpression.left), addTypesIntoExpression(calculatorExpression.right))
      .map { case (typedLeft, typedRight) =>
        calculatorExpression.copy(
          left = typedLeft,
          right = typedRight,
          returnType = number
        )
      }
  }

  def addTypesIntoNotExpression(not: Not): Validated[Expression] = {
    addTypesIntoExpression(not.inner).map { expression =>
      not.copy(
        inner = expression,
        returnType = boolean
      )
    }
  }

  def addTypesIntoReference(reference: Reference): Validated[Reference] = {
    context.findTypeReference(reference.name) match {
      case Some(typeReference) => Valid(reference.copy(returnType = typeReference))
      case None => Invalid("Unknown reference: " + reference.name, reference.location)
    }
  }

  def addTypeIntoMethodCall(methodCall: MethodCall): Validated[MethodCall] = {
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
                  Valid(methodCall.copy(
                    expression = expression,
                    parameters = parameters,
                    returnType = returnTypeOfMethod(typedMethodDefinition, parameters)
                  ))
                case None => Invalid(s"Unknown method ${typeReference.typeName}.${methodCall.method}", methodCall.location)
              }
            }
          case other => Invalid(s"Expected type, got ${other.readableString}", expression.location)
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
            case Unset => Unset
          }
        )
      },
      returnType = adaptTypeReference(methodDefinition.returnType)
    )
  }

  private def classReferenceToTypeReference(classReference: ClassReference): TypeReference = {
    TypeReference(
      typeName = classReference.classDefinition.fullName,
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

  def getMethodOpt(classDefinition: ClassDefinition, method: String)(implicit context: Context): Option[MethodDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.methods.find(_.name == method)
      case aliasType: AliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getMethodOpt(_, method))
      case _ => None
    }
  }

  def addTypeIntoAttributeCall(attributeCall: AttributeCall): Validated[AttributeCall] = {
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
                Valid(attributeCall.copy(
                  expression = expression,
                  returnType = typeDeclarationToTypeReference(attributeDefinition.typeDeclaration)
                ))
              case None => Invalid(s"Unknown attribute ${typeReference.typeName}.${attributeCall.attribute}", attributeCall.location)
            }
          }
        case other => Invalid(s"Expected type, got ${other.readableString}", expression.location)
      }
    }
  }

  private def typeDeclarationToTypeReference(typeDeclaration: TypeDeclaration): TypeReference = {
    TypeReference(
      typeName = typeDeclaration.typeName,
      genericTypes = typeDeclaration.genericTypes.map(typeDeclarationToTypeReference)
    )
  }

  def getAttributeOpt(classDefinition: ClassDefinition, attribute: String)(implicit context: Context): Option[AttributeDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.attributes.find(_.name == attribute)
      case aliasType: AliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getAttributeOpt(_, attribute))
      case definedType: DefinedType =>
        definedType.attributes.find(_.name == attribute)
      case enum: Enum =>
        enum.cases
          .find(_.name == attribute)
          .map { enumCase =>
            // Actually this is quite a little workaround because it is not really an attribute with all their feature.
            // But it avoid rewriting lot of code outside just to consider enum and their cases.
            AttributeDefinition(
              name = enumCase.name,
              typeDeclaration = TypeDeclaration(
                enum.fullName,
                genericTypes = Seq.empty,
                parameters = Seq.empty,
                location = enum.location
              ),
              comment = enumCase.comment,
              verifications = Seq.empty,
              typeName = None,
              location = enumCase.location
            )
          }
    }
  }

  def addTypeIntoCombinedExpression(combinedExpression: CombinedExpression): Validated[CombinedExpression] = {
    Validated
      .squash(combinedExpression.parts.map(addTypesIntoExpression))
      .map { parts =>
        combinedExpression.copy(
          parts = parts,
          returnType = parts.last.returnType
        )
      }
  }

  def addTypeIntoCondition(condition: Condition): Validated[Condition] = {
    val validatedTypedCondition = addTypesIntoExpression(condition.condition)
    val validatedTypedOnTrue = addTypesIntoExpression(condition.onTrue)
    val validatedTypeOnFalse = Validated.reverseOption(condition.onFalse.map(addTypesIntoExpression))
    Validated
      .both(validatedTypedCondition, validatedTypedOnTrue, validatedTypeOnFalse)
      .map { case (conditionExpression, onTrue, onFalse) =>
        val returnType = if (onFalse.exists(_.returnType == onTrue.returnType)) {
          onTrue.returnType
        } else {
          unit
        }
        condition.copy(
          condition = conditionExpression,
          onTrue = onTrue,
          onFalse = onFalse,
          returnType = returnType
        )
      }
  }

  def addTypeIntoLambdaExpression(lambdaExpression: LambdaExpression): Validated[LambdaExpression] = {
    val lambdaContext = LambdaContext(context, lambdaExpression)
    val lambdaExpressionTyping = new ExpressionTyping(lambdaContext)
    lambdaExpressionTyping.addTypesIntoExpression(lambdaExpression.expression)
      .map { expression =>
        lambdaExpression.copy(
          expression = expression,
          returnType = expression.returnType
        )
      }
  }

  def addTypeIntoFunctionCall(functionCall: FunctionCall): Validated[FunctionCall] = {
    context.findFunction(functionCall.name) match {
      case Some(namedFunction) =>
        val validatedTypedParameters = Validated.squash(functionCall.parameters.map(addTypesIntoExpression))
        validatedTypedParameters.map { parameters =>
          functionCall.copy(
            parameters = parameters,
            returnType = namedFunction.returnType
          )
        }
      case None =>
        Invalid(s"Unknown function ${functionCall.name}", functionCall.location)
    }
  }

  private def addTypesIntoOkValue(okValue: OkValue): Validated[OkValue] = {
    Valid(okValue.copy(returnType = okko))
  }

  private def addTypesIntoKoValue(koValue: KoValue): Validated[KoValue] = {
    val validatedTypedParameters = Validated.squash(koValue.parameters.map(addTypesIntoExpression))
    validatedTypedParameters.map { parameters =>
      koValue.copy(
        parameters = parameters,
        returnType = okko
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
  val unit = TypeReference("unit", Seq.empty)
}