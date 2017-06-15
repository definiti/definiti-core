package definiti.core

import java.nio.file.Path

import definiti.core.validation.Validation

trait Plugin {
  def name: String
}

trait ParserPlugin extends Plugin {
  def transform(root: Root): Either[Seq[String], Root]
}

trait ValidatorPlugin extends Plugin {
  def validate(root: Root, context: Context): Validation
}

trait GeneratorPlugin extends Plugin {
  def generate(root: Root, context: Context): Map[Path, String]
}