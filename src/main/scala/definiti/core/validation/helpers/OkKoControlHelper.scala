package definiti.core.validation.helpers

import definiti.common.ast._
import definiti.common.control.{Control, ControlResult}
import definiti.common.validation.Alert

private[core] trait OkKoControlHelper {
  self: Control[Root] with TypeReferenceControlHelper =>

  def controlTypes(expectedTypes: Seq[TypeReference], gotExpressions: Seq[Expression], location: Location, library: Library): ControlResult = {
    if (expectedTypes.length == gotExpressions.length) {
      ControlResult.squash {
        expectedTypes.zip(gotExpressions)
          .map { case (expectedParameter, gotExpression) => controlType(expectedParameter, gotExpression.returnType, gotExpression.location, library) }
      }
    } else {
      invalidNumberOfParameters(expectedTypes.length, gotExpressions.length, location)
    }
  }

  def controlType(expectedType: TypeReference, gotType: AbstractTypeReference, location: Location, library: Library): ControlResult = {
    gotType match {
      case gotTypeReference: TypeReference if areTypeEqual(expectedType, gotTypeReference, library) => OK
      case _ => invalidType(expectedType, gotType, location)
    }
  }

  def invalidNumberOfParameters(expected: Int, got: Int, location: Location): Alert = {
    alert(
      s"Expected ${expected} parameters, got ${got}",
      location
    )
  }

  def invalidType(expected: TypeReference, got: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Expected type ${expected.readableString}, got ${got.readableString}",
      location
    )
  }
}