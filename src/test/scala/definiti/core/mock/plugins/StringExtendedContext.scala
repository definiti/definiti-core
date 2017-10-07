package definiti.core.mock.plugins

import definiti.core.ast.Library
import definiti.core.{ContextPlugin, Valid, ast}

class StringExtendedContext extends ContextPlugin[String] {
  override def contextName = "stringContext"

  override def parse(content: String, range: ast.Range): String = content

  override def validate(context: String, library: Library): Valid.type = Valid

  override def name = "stringContext"
}
