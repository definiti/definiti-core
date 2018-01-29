package definiti.core

object Boot {
  def main(args: Array[String]): Unit = {
    val configuration = new FileConfiguration()
    val project = new Project(configuration)
    project.program().run(configuration) match {
      case Ko(alerts) =>
        System.err.println("Aborted with errors")
        alerts.foreach { alert =>
          System.err.println(alert.prettyPrint)
        }
      case Ok(_, alerts) =>
        println("Done without error")
        alerts.foreach { alert =>
          println(alert.prettyPrint)
        }
    }
  }
}
