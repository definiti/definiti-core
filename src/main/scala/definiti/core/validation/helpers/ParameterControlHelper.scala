package definiti.core.validation.helpers

import definiti.common.ast._
import definiti.common.control.{Control, ControlResult}
import definiti.common.validation.Alert

private[core] trait ParameterControlHelper {
  self: Control[Root] with TypeReferenceControlHelper =>

  def controlParameters(expectedParameters: Seq[ParameterDefinition], gotParameters: Seq[Expression], library: Library, location: Location): ControlResult = {
    if (expectedParameters.length == gotParameters.length) {
      ControlResult.squash {
        expectedParameters.zip(gotParameters)
          .map { case (expectedParameter, gotParameter) => controlParameter(expectedParameter, gotParameter, library) }
      }
    } else {
      invalidNumberOfParameters(expectedParameters.length, gotParameters.length, location)
    }
  }

  def controlParameter(expectedParameter: ParameterDefinition, callExpression: Expression, library: Library): ControlResult = {
    (expectedParameter.typeReference, callExpression) match {
      case (expectedLambda: LambdaReference, gotLambda: LambdaExpression) =>
        validateLambda(expectedLambda, gotLambda, library)
      case (expectedLambda: LambdaReference, expression) =>
        expression.returnType match {
          case reference: NamedFunctionReference =>
            controlNamedFunctionReference(reference, expectedLambda, library, callExpression.location)
          case _ =>
            invalidParameterType(expectedLambda, expression.returnType, expression.location)
        }
      case _ if areTypeEqual(expectedParameter.typeReference, callExpression.returnType, library) =>
        OK
      case _ =>
        invalidParameterType(expectedParameter.typeReference, callExpression.returnType, callExpression.location)
    }
  }

  private def validateLambda(expectedLambda: LambdaReference, gotLambda: LambdaExpression, library: Library): ControlResult = {
    if (expectedLambda.inputTypes.length == gotLambda.parameterList.length) {
      ControlResult.squash {
        expectedLambda.inputTypes.zip(gotLambda.parameterList)
          .map { case (expectedType, gotParameter) =>
            controlTypeEquality(expectedType, gotParameter.typeReference, gotParameter.location, library)
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
          if (expectedInputs.zip(gotInputs.map(_.typeReference)).forall(x => areTypeEqual(x._1, x._2, library))) {
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

  def invalidParameterType(expected: AbstractTypeReference, got: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Expected type ${expected.readableString}, got ${got.readableString}",
      location
    )
  }

  def invalidNumberOfParameters(expected: Int, got: Int, location: Location): Alert = {
    alert(
      s"Expected ${expected} parameters, got ${got}",
      location
    )
  }

  def unknownFunctionError(function: String, location: Location): Alert = {
    alert(
      s"Unknown function: ${function}",
      location
    )
  }
}
