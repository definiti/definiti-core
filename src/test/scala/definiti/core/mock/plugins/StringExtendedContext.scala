package definiti.core.mock.plugins

import definiti.core.ast.{Library, Location}
import definiti.core.{ContextPlugin, Valid}

class StringExtendedContext extends ContextPlugin[String] {
  override def contextName = "stringContext"

  override def parse(content: String, location: Location): String = content

  override def validate(context: String, library: Library): Valid.type = Valid

  override def name = "stringContext"

  override def contextToJson(context: String) = context

  override def contextFromJson(json: String) = json
}
