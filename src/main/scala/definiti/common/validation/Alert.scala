package definiti.common.validation

import definiti.common.ast.Location

sealed trait Alert {
  def prettyPrint: String
}

case class AlertControl(control: String, message: String, location: Location) extends Alert {
  override def prettyPrint: String = s"[${control}] ${location.prettyPrint}: ${message}"
}

case class AlertLocation(message: String, location: Location) extends Alert {
  override def prettyPrint: String = s"${location.prettyPrint}: ${message}"
}

case class AlertSimple(message: String) extends Alert {
  override def prettyPrint: String = message
}

object Alert {
  def apply(error: Error): Alert = error match {
    case ASTError(message, location) => AlertLocation(message, location)
    case SimpleError(message) => AlertSimple(message)
  }
}
