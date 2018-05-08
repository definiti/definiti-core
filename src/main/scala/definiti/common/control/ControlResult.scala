package definiti.common.control

import definiti.common.ast.Location
import definiti.common.validation.{Alert, AlertControl}

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

  implicit def autoSquashControlResults(results: Seq[ControlResult]): ControlResult = {
    squash(results)
  }
}