package definiti.core

import java.nio.file.Path

import definiti.core.ast.pure.Root

trait Plugin {
  def name: String
}

trait ParserPlugin extends Plugin {
  def transform(root: Root): Validated[Root]
}

trait ValidatorPlugin extends Plugin {
  def validate(root: Root, context: Context): Validation
}

trait GeneratorPlugin extends Plugin {
  def generate(root: Root, context: Context): Map[Path, String]
}

trait ContextPlugin[A] extends Plugin {
  def contextName: String

  def parse(content: String, range: definiti.core.ast.Range): A

  def validate(context: A)(implicit outerContext: Context): Validation
}