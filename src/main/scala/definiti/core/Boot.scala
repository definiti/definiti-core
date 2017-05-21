package definiti.core

import java.nio.file.{Path, Paths}

import org.kiama.output.PrettyPrinter._

object Boot {
  def main(args: Array[String]): Unit = {
    try {
      val configuration = Configuration(
        source = getSource(args),
        core = CoreConfiguration(
          source = getCoreSource(args)
        )
      )

      val project = new Project(configuration)
      project.load() match {
        case Left(errors) =>
          errors.foreach(System.err.println)
        case Right(projectResult) =>
          println("Done without error. Generated AST is:")
          val root = projectResult.root
          val prettyRoot = root.copy(
            files = List(root.files.map(rootFile => {
              rootFile.copy(
                verifications = List(rootFile.verifications: _*),
                classDefinitions = List(rootFile.classDefinitions: _*)
              )
            }): _*)
          )
          println(pretty(any(prettyRoot)))
      }
    } catch {
      // In some cases, an Exception is thrown because the parser do not recognize an expression and crash its tree.
      // Did not happened with a successful syntax yet.
      case e: Exception =>
        e.printStackTrace()
    }
  }

  def getSource(args: Array[String]): Path = {
    args.find(_.startsWith("source="))
      .map(_.drop("source=".length))
      .map(Paths.get(_))
      .getOrElse(Paths.get("src", "main", "resources", "samples", "first.def"))
  }

  def getCoreSource(args: Array[String]): Path = {
    args.find(_.startsWith("destination="))
      .map(_.drop("destination=".length))
      .map(Paths.get(_))
      .getOrElse(Paths.get("src", "main", "resources", "api"))
  }
}
