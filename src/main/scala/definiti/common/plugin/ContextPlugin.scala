package definiti.common.plugin

import definiti.common.ast.{Library, Location}
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation.Validated

trait ContextPlugin[A] extends Plugin {
  def contextName: String

  def parse(content: String, packageName: String, imports: Map[String, String], location: Location): A

  def validate(context: A, library: Library): Validated[NoResult]

  def contextToJson(context: A): String

  def contextFromJson(json: String): A
}
