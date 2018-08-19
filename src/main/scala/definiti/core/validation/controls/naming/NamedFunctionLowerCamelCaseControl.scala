package definiti.core.validation.controls.naming

import definiti.common.ast.{Library, NamedFunction, Root}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.NameFormatHelper

object NamedFunctionLowerCamelCaseControl extends Control[Root] with NameFormatHelper {
  override def description: String = "Functions must be in lowerCamelCase format"

  override def defaultLevel: ControlLevel.Value = ControlLevel.warning

  override def control(value: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.namedFunctions.map(controlNamedFunction)
    }
  }

  private def controlNamedFunction(namedFunction: NamedFunction): ControlResult = {
    controlLowerCamelCaseFormat(namedFunction.name, namedFunction.location)
  }
}
