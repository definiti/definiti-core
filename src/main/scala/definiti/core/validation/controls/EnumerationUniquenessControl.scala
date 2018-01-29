package definiti.core.validation.controls

import definiti.core.ast._

object EnumerationUniquenessControl extends Control {
  override val name: String = "enumerationUniqueness"
  override val description: String = "Check if every variable in enumeration is unique"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.enums.map(controlEnum)
    }
  }

  private def controlEnum(enum: Enum): ControlResult = {
    ControlResult.squash {
      enum.cases.zipWithIndex.collect {
        case (enumCase, index) if enum.cases.indexOf(enumCase) != index =>
          alert(s"The enum ${enumCase.name} is already defined in enum ${enum.name}", enumCase.location)
      }
    }
  }
}
