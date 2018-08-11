package definiti.core.validation.controls

import definiti.common.ast.{Library, Root, TypeReference}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.TypeReferenceControlHelper

object NamedFunctionParameterTypeControl extends Control[Root] with TypeReferenceControlHelper {
  override def description: String = "Control types of parameters of named functions"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      for {
        namedFunction <- library.namedFunctions
        parameter <- namedFunction.parameters
      } yield {
        parameter.typeReference match {
          case typeReference: TypeReference =>
            controlTypeReference(
              typeReference = typeReference,
              availableGenerics = namedFunction.genericTypes,
              location = parameter.location,
              library = library
            )

          case _ =>
            OK
        }
      }
    }
  }
}
