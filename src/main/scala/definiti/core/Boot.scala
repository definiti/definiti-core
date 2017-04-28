package definiti.core

import java.nio.file.Paths

object Boot extends App {
  try {
    val configuration = Configuration(
      source = Paths.get("src", "main", "resources", "samples", "first.def"),
      core = CoreConfiguration(
        source = Paths.get("src", "main", "resources", "api")
      )
    )

    val project = new Project(configuration)
    project.load() match {
      case Left(errors) =>
        errors.foreach(System.err.println)
      case Right(root) =>
        println("Done without error. Generated AST is:")
        println(root)
    }
  } catch {
    // In some cases, an Exception is thrown because the parser do not recognize an expression and crash its tree.
    // Did not happened with a successful syntax yet.
    case e: Exception =>
      e.printStackTrace()
  }
}
