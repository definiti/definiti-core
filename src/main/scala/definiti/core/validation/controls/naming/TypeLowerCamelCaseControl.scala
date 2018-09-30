package definiti.core.validation.controls.naming

import definiti.common.ast.{DefinedType, Library, ProjectClassDefinition, Root}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.NameFormatHelper

object TypeLowerCamelCaseControl extends Control[Root] with NameFormatHelper {
  override def description: String = "Type names must be in lowerCamelCase format"

  override def defaultLevel: ControlLevel.Value = ControlLevel.ignored

  override def control(value: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.types
        .collect {
          case classDefinition: ProjectClassDefinition => controlType(classDefinition)
        }
    }
  }

  private def controlType(classDefinition: ProjectClassDefinition): ControlResult = {
    classDefinition match {
      case definedType: DefinedType =>
        val nameControl = controlLowerCamelCaseFormat(definedType.name, definedType.location)
        val attributeTypeControls = definedType.attributes.flatMap { attribute =>
          attribute.attributeType.map(x => controlLowerCamelCaseFormat(x.name, attribute.location))
        }
        nameControl +: attributeTypeControls

      case other =>
        controlLowerCamelCaseFormat(other.name, other.location)
    }
  }
}
