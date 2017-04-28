package definiti.core

import java.nio.file.Path

case class Configuration(
  source: Path,
  core: CoreConfiguration
)

case class CoreConfiguration(
  source: Path
)