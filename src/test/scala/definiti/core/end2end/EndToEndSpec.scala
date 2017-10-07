package definiti.core.end2end

import java.nio.file.Paths

import definiti.core._
import definiti.core.ast.Root
import definiti.core.mock.plugins.StringExtendedContext
import org.scalatest.{FlatSpec, Matchers}

trait EndToEndSpec extends FlatSpec with Matchers {
  def processDirectory(sample: String): Validated[Root] = {
    val project = new Project(configurationDirectory(sample))
    project.generatePublicAST()
  }

  def configurationDirectory(sample: String): Configuration = {
    ConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = Seq(new StringExtendedContext())
    )
  }

  def processFile(sample: String): Validated[Root] = {
    val project = new Project(configurationFile(sample))
    project.generatePublicAST()
  }

  def configurationFile(sample: String): Configuration = {
    ConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = Seq(new StringExtendedContext())
    )
  }
}
