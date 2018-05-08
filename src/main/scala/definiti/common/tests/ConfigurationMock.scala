package definiti.common.tests

import java.nio.file.{Path, Paths}

import definiti.common.control.{Control, ControlLevel}
import definiti.common.plugin._
import definiti.common.program.ProgramConfiguration
import definiti.core.Configuration
import definiti.core.validation.Controls

case class ConfigurationMock(
  source: Path = Paths.get(""),
  apiSource: Path = Paths.get(""),
  parsers: Seq[ParserPlugin] = Seq.empty,
  validators: Seq[ValidatorPlugin] = Seq.empty,
  generators: Seq[GeneratorPlugin] = Seq.empty,
  contexts: Seq[ContextPlugin[_]] = Seq.empty,
  controlLevel: ControlLevel.Value = ControlLevel.warning,
  fatalLevel: ControlLevel.Value = ControlLevel.error,
  userFlags: Map[String, ControlLevel.Value] = Map.empty
) extends Configuration {
  def programConfiguration: ProgramConfiguration = ProgramConfiguration(
    controlLevel = controlLevel,
    fatalLevel = fatalLevel,
    userFlags = userFlags,
    defaultLevels = {
      Controls.all
        .map { control => control.name -> control.defaultLevel }
        .toMap
    }
  )

  def withOnlyControls(controls: String*): ConfigurationMock = {
    copy(
      userFlags = Controls.all.map { control =>
        control.name -> (if (controls.contains(control.name)) ControlLevel.error else ControlLevel.ignored)
      }.toMap
    )
  }

  def withOnlyControls(controls: Control*)(implicit dummyImplicit: DummyImplicit): ConfigurationMock = {
    withOnlyControls(controls.map(_.name): _*)
  }
}