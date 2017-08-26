package definiti.core

object Boot {
  def main(args: Array[String]): Unit = {
    val configuration = new FileConfiguration()
    val project = new Project(configuration)
    project.process() match {
      case Invalid(errors) => errors.foreach(error => System.err.println(error.prettyPrint))
      case Valid => println("Done without error")
    }
  }
}
