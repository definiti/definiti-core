package definiti.common.tests

import java.nio.file.Paths

import definiti.common.ast.Root
import definiti.common.program.ProgramResult
import definiti.core.{Configuration, Project}

trait TestProjectExecution {
  def processDirectory(sample: String, configuration: ConfigurationMock = ConfigurationMock()): ProgramResult[Root] = {
    val finalConfiguration = configurationDirectory(sample, configuration)
    val project = new Project(finalConfiguration)
    project.generatePublicAST().run(finalConfiguration.programConfiguration)
  }

  def configurationDirectory(sample: String, configuration: ConfigurationMock): Configuration = {
    configuration.copy(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = configuration.contexts :+ new DummyExtendedContext()
    )
  }

  def processFile(sample: String, configuration: ConfigurationMock = ConfigurationMock()): ProgramResult[Root] = {
    val finalConfiguration = configurationFile(sample, configuration)
    val project = new Project(finalConfiguration)
    project.generatePublicAST().run(finalConfiguration.programConfiguration)
  }

  def configurationFile(sample: String, configuration: ConfigurationMock): Configuration = {
    configuration.copy(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/test/resources/core"),
      contexts = configuration.contexts :+ new DummyExtendedContext()
    )
  }
}