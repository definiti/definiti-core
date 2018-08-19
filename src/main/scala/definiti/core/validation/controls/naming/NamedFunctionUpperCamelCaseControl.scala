package definiti.core.validation.controls.naming

import definiti.common.ast.{Library, NamedFunction, Root}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.NameFormatHelper

object NamedFunctionUpperCamelCaseControl extends Control[Root] with NameFormatHelper {
  override def description: String = "Functions must be in UpperCamelCase format"

  override def defaultLevel: ControlLevel.Value = ControlLevel.ignored

  override def control(value: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.namedFunctions.map(controlNamedFunction)
    }
  }

  private def controlNamedFunction(namedFunction: NamedFunction): ControlResult = {
    controlUpperCamelCaseFormat(namedFunction.name, namedFunction.location)
  }
}
