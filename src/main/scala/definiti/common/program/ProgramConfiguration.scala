package definiti.common.program

import definiti.common.control.{Control, ControlLevel}

case class ProgramConfiguration(
  controlLevel: ControlLevel.Value,
  fatalLevel: ControlLevel.Value,
  userFlags: Map[String, ControlLevel.Value],
  defaultLevels: Map[String, ControlLevel.Value]
) {
  def levelOfControl(controlName: String): Option[ControlLevel.Value] = {
    userFlags.get(controlName).orElse(defaultLevels.get(controlName))
  }

  def isControlAccepted[_](control: Control[_]): Boolean = {
    levelOfControl(control.name).getOrElse(ControlLevel.ignored) >= controlLevel
  }
}