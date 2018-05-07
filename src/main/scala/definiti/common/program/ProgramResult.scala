package definiti.common.program

import definiti.common.validation.Alert

sealed trait ProgramResult[A] {
  def prettyPrint: String
}

object ProgramResult {

  sealed trait NoResult

  object NoResult extends NoResult

}

case class Ok[A](value: A, alerts: Seq[Alert]) extends ProgramResult[A] {
  override def prettyPrint: String = s"Ok(\n  ${value.toString},${sortedStringAlerts.mkString("\n  ", "\n  ", "\n")})"

  private lazy val sortedStringAlerts: Seq[String] = alerts.map(_.prettyPrint).sorted
}

object Ok {
  def apply[A](value: A, alerts: Alert*)(implicit dummyImplicit: DummyImplicit): Ok[A] = new Ok(value, alerts)
}

case class Ko[A](alerts: Seq[Alert]) extends ProgramResult[A] {
  override def prettyPrint: String = s"Ko(${sortedStringAlerts.mkString("\n  ", "\n  ", "\n")})"

  private lazy val sortedStringAlerts: Seq[String] = alerts.map(_.prettyPrint).sorted
}

object Ko {
  def apply[A](alerts: Alert*)(implicit dummyImplicit: DummyImplicit): Ko[A] = new Ko(alerts)
}
