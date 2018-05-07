package definiti.core

import java.nio.file.{Path, Paths}

import definiti.common.plugin._
import definiti.common.program.ProgramConfiguration
import definiti.common.control.ControlLevel
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
}