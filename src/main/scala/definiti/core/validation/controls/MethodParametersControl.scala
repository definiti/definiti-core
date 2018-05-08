package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.{ExpressionControlHelper, ParameterControlHelper, TypeReferenceControlHelper}

private[core] object MethodParametersControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper with ParameterControlHelper {
  override val description: String = "Check if parameters and arguments on a method are the same"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    testAllExpressions(library) { expression =>
      deepControl(expression) {
        case methodCall: MethodCall => controlMethodCall(methodCall, library)
      }
    }
  }

  private def controlMethodCall(methodCall: MethodCall, library: Library): ControlResult = {
    val result = for {
      returnTypeReference <- getTypeReference(methodCall.expression)
      classDefinition <- getClassDefinition(returnTypeReference, library, methodCall.location)
      rawMethod <- getMethod(classDefinition, methodCall, library)
      method = updateTypesInMethod(rawMethod, classDefinition, methodCall, returnTypeReference)
    } yield {
      controlParameters(method.parameters, methodCall.parameters, library, methodCall.location)
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

  private def updateTypesInMethod(methodDefinition: MethodDefinition, classDefinition: ClassDefinition, methodCall: MethodCall, returnTypeReference: TypeReference): MethodDefinition = {
    def updateType(innerType: TypeReference): TypeReference = {
      if (classDefinition.genericTypes.contains(innerType.typeName)) {
        returnTypeReference.genericTypes(classDefinition.genericTypes.indexOf(innerType.typeName))
      } else if (methodDefinition.genericTypes.contains(innerType.typeName)) {
        methodCall.generics(methodDefinition.genericTypes.indexOf(innerType.typeName))
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

  def unexpectedTypeError(typeReference: AbstractTypeReference, location: Location): Alert = {
    alert(s"Unexpected type: ${typeReference.readableString}", location)
  }

  def unknownTypeError(typeReference: AbstractTypeReference, location: Location): Alert = {
    alert(s"Unknown type: ${typeReference.readableString}", location)
  }

  def unknownMethodError(typeName: String, method: String, location: Location): Alert = {
    alert(s"Unknown method: ${typeName}.${method}", location)
  }
}
