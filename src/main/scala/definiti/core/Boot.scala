package definiti.core

import definiti.common.program._
import definiti.common.validation.{Alert, AlertControl, AlertLocation, AlertSimple}

object Boot {
  def main(args: Array[String]): Unit = {
    val configuration = new FileConfiguration()
    val project = new Project(configuration)
    project.program().run(configuration.programConfiguration) match {
      case Ko(alerts) =>
        System.err.println("Aborted with errors")
        printAlerts(alerts, configuration.programConfiguration)
      case Ok(_, alerts) =>
        println("Done without error")
        printAlerts(alerts, configuration.programConfiguration)
    }
  }

  private def printAlerts(alerts: Seq[Alert], configuration: ProgramConfiguration): Unit = {
    alerts.foreach {
      case alert@(_: AlertSimple | _: AlertLocation) => System.err.println(alert.prettyPrint)
      case alert: AlertControl =>
        configuration.levelOfControl(alert.control) match {
          case Some(level) if level >= configuration.fatalLevel => System.err.println(alert.prettyPrint)
          case _ => System.out.println(alert.prettyPrint)
        }
    }
  }
}
