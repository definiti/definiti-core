package definiti.core.parser.project

import definiti.common.ast.NamespaceElement

case class FileContent(
  filename: String,
  packageName: String,
  imports: Map[String, String],
  elements: Seq[NamespaceElement]
)