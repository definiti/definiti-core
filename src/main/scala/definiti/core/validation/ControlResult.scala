package definiti.core.validation

import definiti.core.ast.Location
import definiti.core.{Alert, AlertControl}

case class ControlResult(alerts: Seq[Alert]) {
  def +(control: ControlResult): ControlResult = ControlResult(alerts ++ control.alerts)
}

object ControlResult {
  def apply(alerts: Alert*)(implicit dummyImplicit: DummyImplicit): ControlResult = new ControlResult(alerts)

  val OK = ControlResult(Seq.empty)

  def alert(control: String, message: String, location: Location): ControlResult = {
    ControlResult(Seq(AlertControl(control, message, location)))
  }

  def squash(results: Seq[ControlResult]): ControlResult = {
    ControlResult(results.flatMap(_.alerts))
  }
}