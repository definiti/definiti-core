package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert

private[core] object EnumerationUniquenessControl extends Control[Root] {
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
