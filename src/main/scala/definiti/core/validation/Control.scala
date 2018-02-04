package definiti.core.validation

import definiti.core.ast.{Library, Location, Root}
import definiti.core.{Alert, AlertControl}

trait Control {
  val name: String = {
    val simpleName = this.getClass.getSimpleName
    val normalizedName = simpleName.substring(0, simpleName.length - "Control$".length)
    normalizedName.charAt(0).toLower + normalizedName.substring(1)
  }

  def description: String

  def defaultLevel: ControlLevel.Value

  def control(root: Root, library: Library): ControlResult

  val OK: ControlResult = ControlResult.OK

  /** Means the control should be done in another class and is simply ignored here */
  val ignored: ControlResult = ControlResult.OK

  def alert(message: String, location: Location): Alert = AlertControl(name, message, location)

  protected implicit def alertToControlResult(alert: Alert): ControlResult = ControlResult(alert)
}

object ControlLevel extends Enumeration {
  val ignored, info, warning, error = Value

  def fromString(value: String): Option[ControlLevel.Value] = {
    values.find(_.toString == value)
  }
}