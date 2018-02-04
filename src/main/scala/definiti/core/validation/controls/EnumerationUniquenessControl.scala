package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}

object EnumerationUniquenessControl extends Control {
  override val description: String = "Check if each case of enumeration is unique"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.enums.map(controlEnum)
    }
  }

  private def controlEnum(enum: Enum): ControlResult = {
    ControlResult {
      enum.cases.zipWithIndex.collect {
        case (enumCase, index) if enum.cases.indexWhere(_.name == enumCase.name) != index =>
          errorDuplication(enumCase.name, enum.name, enumCase.location)
      }
    }
  }

  def errorDuplication(enumCase: String, enumName: String, location: Location): Alert = {
    alert(s"The enum ${enumCase} is already defined in enum ${enumName}", location)
  }
}
