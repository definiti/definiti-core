package definiti.core.validation.controls

import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.TypeReferenceControlHelper

object AliasTypeTypeControl extends Control with TypeReferenceControlHelper {
  override val description: String = "Check if types of alias type is valid"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.aliasTypes.map(controlAliasType(_, library))
    }
  }

  private def controlAliasType(aliasType: AliasType, library: Library): ControlResult = {
    controlTypeReference(
      typeReference = aliasType.alias,
      elementName = aliasType.fullName,
      availableGenerics = aliasType.genericTypes,
      location = aliasType.location,
      library = library
    )
  }
}
