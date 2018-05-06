package definiti.core.mock.plugins

import definiti.core.ProgramResult.NoResult
import definiti.core.ast.{Library, Location}
import definiti.core.{ContextPlugin, Valid, Validated}

class StringExtendedContext extends ContextPlugin[String] {
  override def contextName = "stringContext"

  override def parse(content: String, packageName: String, imports: Map[String, String], location: Location): String = content

  override def validate(context: String, library: Library): Validated[NoResult] = Valid(NoResult)

  override def name = "stringContext"

  override def contextToJson(context: String) = context

  override def contextFromJson(json: String) = json
}
