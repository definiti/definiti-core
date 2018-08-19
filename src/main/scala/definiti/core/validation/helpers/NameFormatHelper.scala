package definiti.core.validation.helpers

import definiti.common.ast.{Location, Root}
import definiti.common.control.{Control, ControlResult}
import definiti.common.validation.Alert

private[core] trait NameFormatHelper {
  self: Control[Root] =>

  def controlUpperCamelCaseFormat(name: String, location: Location): ControlResult = {
    if (name.headOption.exists(_.isLower)) {
      invalidUpperCamelCaseFormat(name, location)
    } else {
      OK
    }
  }

  def invalidUpperCamelCaseFormat(name: String, location: Location): Alert = {
    alert(s"The name ${name} is not in UpperCamelCase format", location)
  }

  def controlLowerCamelCaseFormat(name: String, location: Location): ControlResult = {
    if (name.headOption.exists(_.isUpper)) {
      invalidLowerCamelCaseFormat(name, location)
    } else {
      OK
    }
  }

  def invalidLowerCamelCaseFormat(name: String, location: Location): Alert = {
    alert(s"The name ${name} is not in lowerCamelCase format", location)
  }
}
