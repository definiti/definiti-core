package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert

private[core] object TypeNameFormatControl extends Control {
  override val description: String = "Check the format of name of every type (UpperCamelCase)"
  override val defaultLevel: ControlLevel.Value = ControlLevel.warning

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.projectTypes.map(controlTypeName)
    }
  }

  private def controlTypeName(clazz: ProjectClassDefinition): ControlResult = {
    if (clazz.name.isEmpty) {
      errorEmptyName(clazz.location)
    } else if (clazz.name.head.isUpper) {
      OK
    } else {
      invalidNameFormat(clazz.name, clazz.location)
    }
  }

  def errorEmptyName(location: Location): Alert = {
    alert(s"The type has an empty name", location)
  }

  def invalidNameFormat(name: String, location: Location): Alert = {
    alert(s"The type ${name} does not start with an upper case letter", location)
  }
}
