package definiti.common.plugin

import java.nio.file.Path

import definiti.common.ast.{Library, Root}

trait GeneratorPlugin extends Plugin {
  def generate(root: Root, library: Library): Map[Path, String]
}

