package definiti.core.end2end

import java.nio.file.Paths

import definiti.core._
import definiti.core.ast.{Location, Root}
import definiti.core.mock.plugins.StringExtendedContext
import org.scalatest.{FlatSpec, Matchers}

trait EndToEndSpec extends FlatSpec with Matchers {
  def processDirectory(sample: String): ProgramResult[Root] = {
    val configuration = configurationDirectory(sample)
    val project = new Project(configuration)
    project.generatePublicAST().run(configuration)
  }

  def configurationDirectory(sample: String): Configuration = {
    ConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = Seq(new StringExtendedContext())
    )
  }

  def processFile(sample: String): ProgramResult[Root] = {
    val configuration = configurationFile(sample)
    val project = new Project(configuration)
    project.generatePublicAST().run(configuration)
  }

  def configurationFile(sample: String): Configuration = {
    ConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = Seq(new StringExtendedContext())
    )
  }
}

object EndToEndSpec {
  case class LocationPath(path: String) {
    def apply(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Location = {
      Location(path, startLine, startColumn, endLine, endColumn)
    }
    def apply(line: Int, startColumn: Int, endColumn: Int): Location = {
      Location(path, line, startColumn, line, endColumn)
    }
  }
}