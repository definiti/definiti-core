package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast.{Library, Location, Root}

trait Control {
  def name: String

  def description: String

  def defaultLevel: ControlLevel.Value

  def control(root: Root, library: Library): ControlResult

  val OK: ControlResult = ControlResult.OK

  /** Means the control should be done in another class and is simply ignored here */
  val ignored: ControlResult = ControlResult.OK

  def alert(message: String, location: Location): ControlResult = ControlResult.alert(name, message, location)

  protected implicit def alertToControlResult(alert: Alert): ControlResult = ControlResult(alert)
}

object ControlLevel extends Enumeration {
  val ignored, info, warning, error = Value

  def fromString(value: String): Option[ControlLevel.Value] = {
    values.find(_.toString == value)
  }
}