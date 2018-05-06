package definiti.core

import java.nio.file.Path

import definiti.core.ProgramResult.NoResult
import definiti.core.ast.pure.PureRoot
import definiti.core.ast.{Library, Location, Root}

trait Plugin {
  def name: String
}

trait ParserPlugin extends Plugin {
  def transform(root: PureRoot): Validated[PureRoot]
}

trait ValidatorPlugin extends Plugin {
  def validate(root: Root, library: Library): Validated[NoResult]
}

trait GeneratorPlugin extends Plugin {
  def generate(root: Root, library: Library): Map[Path, String]
}

trait ContextPlugin[A] extends Plugin {
  def contextName: String

  def parse(content: String, packageName: String, imports: Map[String, String], location: Location): A

  def validate(context: A, library: Library): Validated[NoResult]

  def contextToJson(context: A): String

  def contextFromJson(json: String): A
}