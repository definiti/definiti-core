package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert

private[core] object AttributeTypeUniquenessControl extends Control[Root] {
  override val description: String = "Check if there is no several attribute types with the same name"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.definedTypes.map(controlDefinedType(_, library))
    }
  }

  private def controlDefinedType(definedType: DefinedType, library: Library): ControlResult = {
    ControlResult.squash {
      definedType.attributes.zipWithIndex.map { case (attribute, index) =>
        attribute.attributeType match {
          case Some(attributeType) =>
            val indexOfAttribute = definedType.attributes.indexWhere(_.attributeType.map(_.name).contains(attributeType.name))
            if (indexOfAttribute != index) {
              val otherAttribute = definedType.attributes(indexOfAttribute)
              ControlResult(errorSameName(attributeType.name, attribute.location, otherAttribute.location))
            } else {
              OK
            }
          case None =>
            OK
        }
      }
    }
  }

  def errorSameName(conflictName: String, location1: Location, location2: Location): Seq[Alert] = {
    Seq(location1, location2).map { location =>
      alert(s"The attribute type name ${conflictName} is used at least twice", location)
    }
  }
}
