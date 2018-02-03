package definiti.core.validation.controls

import definiti.core.ast._
import definiti.core.validation.controls.helpers.TypeReferenceControlHelper

object AliasTypeTypeControl extends Control with TypeReferenceControlHelper {
  override def name: String = "aliasTypeType"

  override def description: String = "Check if types of alias type is valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

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
