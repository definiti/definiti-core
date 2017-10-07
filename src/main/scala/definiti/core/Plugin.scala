package definiti.core

import java.nio.file.Path

import definiti.core.ast.pure.PureRoot
import definiti.core.ast.{Library, Root}

trait Plugin {
  def name: String
}

trait ParserPlugin extends Plugin {
  def transform(root: PureRoot): Validated[PureRoot]
}

trait ValidatorPlugin extends Plugin {
  def validate(root: Root, library: Library): Validation
}

trait GeneratorPlugin extends Plugin {
  def generate(root: Root, library: Library): Map[Path, String]
}

trait ContextPlugin[A] extends Plugin {
  def contextName: String

  def parse(content: String, range: definiti.core.ast.Range): A

  def validate(context: A, library: Library): Validation
}