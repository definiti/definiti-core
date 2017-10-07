package definiti.core.mock.plugins

import definiti.core.{ContextPlugin, Valid, ast}
import definiti.core.ast.structure.Library

class StringExtendedContext extends ContextPlugin[String] {
  override def contextName = "stringContext"

  override def parse(content: String, range: ast.Range): String = content

  override def validate(context: String, library: Library): Valid.type = Valid

  override def name = "stringContext"
}
