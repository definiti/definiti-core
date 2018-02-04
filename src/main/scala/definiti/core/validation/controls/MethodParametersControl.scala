package definiti.core.validation.controls

import definiti.core._
import definiti.core.ast._
import definiti.core.validation.controls.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

object MethodParametersControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper {
  override def name: String = "methodParameters"

  override def description: String = "Check if parameters and arguments on a method are the same"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    testAllExpressions(library) { expression =>
      deepControl(expression) {
        case methodCall: MethodCall => controlMethodCall(methodCall, library)
      }
    }
  }

  private def controlMethodCall(methodCall: MethodCall, library: Library): ControlResult = {
    val result = for {
      typeReference <- getTypeReference(methodCall.expression)
      classDefinition <- getClassDefinition(typeReference, library, methodCall.location)
      rawMethod <- getMethod(classDefinition, methodCall, library)
      method = updateTypesInMethod(rawMethod, classDefinition, typeReference)
    } yield {
      if (method.parameters.length == methodCall.parameters.length) {
        ControlResult.squash {
          method.parameters.zip(methodCall.parameters)
            .map { case (expectedParameter, gotParameter) => controlParameter(expectedParameter, gotParameter, library) }
        }
      } else {
        ControlResult(invalidNumberOfParameters(method.parameters.length, methodCall.parameters.length, methodCall.location))
      }
    }
    result match {
      case Left(alert) => alert
      case Right(controlResult) => controlResult
    }
  }

  private def getTypeReference(expression: Expression): Either[Alert, TypeReference] = {
    expression.returnType match {
      case typeReference: TypeReference => Right(typeReference)
      case _ => Left(unexpectedTypeError(expression.returnType, expression.location))
    }
  }

  private def getClassDefinition(typeReference: TypeReference, library: Library, location: Location): Either[Alert, ClassDefinition] = {
    library.typesMap.get(typeReference.typeName) match {
      case Some(classDefinition) => Right(classDefinition)
      case None => Left(unknownTypeError(typeReference, location))
    }
  }

  private def getMethod(classDefinition: ClassDefinition, methodCall: MethodCall, library: Library): Either[Alert, MethodDefinition] = {
    def process(classDefinition: ClassDefinition): Option[MethodDefinition] = {
      classDefinition match {
        case native: NativeClassDefinition => native.methods.find(_.name == methodCall.method)
        case alias: AliasType => library.typesMap.get(alias.alias.typeName).flatMap(process)
        case _ => None
      }
    }

    process(classDefinition) match {
      case Some(methodDefinition) => Right(methodDefinition)
      case None => Left(unknownMethodError(classDefinition.fullName, methodCall.method, methodCall.location))
    }
  }

  private def updateTypesInMethod(methodDefinition: MethodDefinition, classDefinition: ClassDefinition, typeReference: TypeReference): MethodDefinition = {
    def updateType(innerType: TypeReference): TypeReference = {
      if (classDefinition.genericTypes.contains(innerType.typeName)) {
        typeReference.genericTypes(classDefinition.genericTypes.indexOf(innerType.typeName))
      } else {
        TypeReference(innerType.typeName, innerType.genericTypes.map(updateType))
      }
    }

    methodDefinition.copy(
      parameters = methodDefinition.parameters.map { parameter =>
        parameter.typeReference match {
          case typeReference: TypeReference =>
            parameter.copy(typeReference = updateType(typeReference))
          case lambdaReference: LambdaReference =>
            parameter.copy(
              typeReference = LambdaReference(
                inputTypes = lambdaReference.inputTypes.map(updateType),
                outputType = updateType(lambdaReference.outputType)
              )
            )
          case _ =>
            parameter
        }
      }
    )
  }

  private def controlParameter(expectedParameter: ParameterDefinition, callExpression: Expression, library: Library): ControlResult = {
    (expectedParameter.typeReference, callExpression) match {
      case (expectedLambda: LambdaReference, gotLambda: LambdaExpression) =>
        validateLambda(expectedLambda, gotLambda)
      case (expectedLambda: LambdaReference, expression) =>
        expression.returnType match {
          case reference: NamedFunctionReference =>
            controlNamedFunctionReference(reference, expectedLambda, library, callExpression.location)
          case _ =>
            invalidParameterType(expectedLambda, expression.returnType, expression.location)
        }
      case _ if areTypeEqual(expectedParameter.typeReference, callExpression.returnType) =>
        OK
      case _ =>
        invalidParameterType(expectedParameter.typeReference, callExpression.returnType, callExpression.location)
    }
  }

  private def validateLambda(expectedLambda: LambdaReference, gotLambda: LambdaExpression): ControlResult = {
    if (expectedLambda.inputTypes.length == gotLambda.parameterList.length) {
      ControlResult.squash {
        expectedLambda.inputTypes.zip(gotLambda.parameterList)
          .map { case (expectedType, gotParameter) =>
            controlTypeEquality(expectedType, gotParameter.typeReference, gotParameter.location)
          }
      }
    } else {
      invalidNumberOfParameters(expectedLambda.inputTypes.length, gotLambda.parameterList.length, gotLambda.location)
    }
  }

  private def controlNamedFunctionReference(namedFunctionReference: NamedFunctionReference, expectedLambda: LambdaReference, library: Library, location: Location): ControlResult = {
    library.namedFunctionsMap.get(namedFunctionReference.functionName) match {
      case Some(gotNamedFunction) =>
        val expectedInputs = expectedLambda.inputTypes
        val gotInputs = gotNamedFunction.parameters
        if (expectedInputs.length == gotInputs.length) {
          if (expectedInputs.zip(gotInputs.map(_.typeReference)).forall(x => areTypeEqual(x._1, x._2))) {
            OK
          } else {
            errorTypeEquality(expectedLambda, namedFunctionReference, location)
          }
        } else {
          invalidNumberOfParameters(expectedInputs.length, gotNamedFunction.parameters.length, location)
        }
      case None =>
        unknownFunctionError(namedFunctionReference.functionName, location)
    }
  }

  def unexpectedTypeError(typeReference: AbstractTypeReference, location: Location): Alert = {
    AlertControl(
      name,
      s"Unexpected type: ${typeReference.readableString}",
      location
    )
  }

  def unknownTypeError(typeReference: AbstractTypeReference, location: Location): Alert = {
    AlertControl(
      name,
      s"Unknown type: ${typeReference.readableString}",
      location
    )
  }

  def unknownMethodError(typeName: String, method: String, location: Location): Alert = {
    AlertControl(
      name,
      s"Unknown method: ${typeName}.${method}",
      location
    )
  }

  def invalidNumberOfParameters(expected: Int, got: Int, location: Location): Alert = {
    AlertControl(
      name,
      s"Expected ${expected} parameters, got ${got}",
      location
    )
  }

  def invalidParameterType(expected: AbstractTypeReference, got: AbstractTypeReference, location: Location): Alert = {
    AlertControl(
      name,
      s"Expected type ${expected.readableString}, got ${got.readableString}",
      location
    )
  }

  def unknownFunctionError(function: String, location: Location): Alert = {
    AlertControl(
      name,
      s"Unknown function: ${function}",
      location
    )
  }
}
