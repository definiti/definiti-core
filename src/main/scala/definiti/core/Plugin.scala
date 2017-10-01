package definiti.core

import java.nio.file.Path

trait Plugin {
  def name: String
}

trait ParserPlugin extends Plugin {
  def transform(root: ast.pure.Root): Validated[ast.pure.Root]
}

trait ValidatorPlugin extends Plugin {
  def validate(root: ast.structure.Root, library: ast.structure.Library): Validation
}

trait GeneratorPlugin extends Plugin {
  def generate(root: ast.structure.Root, library: ast.structure.Library): Map[Path, String]
}

trait ContextPlugin[A] extends Plugin {
  def contextName: String

  def parse(content: String, range: definiti.core.ast.Range): A

  def validate(context: A, library: ast.structure.Library): Validation
}