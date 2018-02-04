package definiti.core

object Boot {
  def main(args: Array[String]): Unit = {
    val configuration = new FileConfiguration()
    val project = new Project(configuration)
    project.program().run(configuration) match {
      case Ko(alerts) =>
        System.err.println("Aborted with errors")
        printAlerts(alerts, configuration)
      case Ok(_, alerts) =>
        println("Done without error")
        printAlerts(alerts, configuration)
    }
  }

  private def printAlerts(alerts: Seq[Alert], configuration: Configuration): Unit = {
    alerts.foreach {
      case alert@(_: AlertSimple | _: AlertLocation) => System.err.println(alert.prettyPrint)
      case alert: AlertControl =>
        configuration.controlLevels.get(alert.control) match {
          case Some(level) if level >= configuration.fatalLevel => System.err.println(alert.prettyPrint)
          case _ => System.out.println(alert.prettyPrint)
        }
    }
  }
}
