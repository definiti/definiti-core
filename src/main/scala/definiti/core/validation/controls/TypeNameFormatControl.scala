package definiti.core.validation.controls

import definiti.core.ast._

object TypeNameFormatControl extends Control {
  override val name: String = "typeNameFormat"
  override val description: String = "Check the format of name of every type (UpperCamelCase)"
  override val defaultLevel: ControlLevel.Value = ControlLevel.warning

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.projectTypes.map(controlTypeName)
    }
  }

  private def controlTypeName(clazz: ProjectClassDefinition): ControlResult = {
    if (clazz.name.isEmpty) {
      alert("The type has an empty name", clazz.location)
    } else if (clazz.name.head.isUpper) {
      OK
    } else {
      alert(s"The type ${clazz.name} does not start with an upper case letter", clazz.location)
    }
  }
}
