package definiti.core.validation.controls

import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.TypeReferenceControlHelper

object AttributeTypeControl extends Control with TypeReferenceControlHelper {
  override val description: String = "Check if types of defined type is valid"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.definedTypes.map(controlDefinedType(_, library))
    }
  }

  private def controlDefinedType(definedType: DefinedType, library: Library): ControlResult = {
    ControlResult.squash {
      definedType.attributes.map(controlAttribute(_, definedType, library))
    }
  }

  private def controlAttribute(attributeDefinition: AttributeDefinition, definedType: DefinedType, library: Library): ControlResult = {
    controlTypeReference(
      typeReference = attributeDefinition.typeDeclaration,
      elementName = s"${definedType.fullName}.${attributeDefinition.name}",
      availableGenerics = definedType.genericTypes,
      location = attributeDefinition.location,
      library = library
    )
  }
}
