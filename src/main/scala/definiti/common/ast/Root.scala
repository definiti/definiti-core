package definiti.common.ast

case class Root(
  elements: Seq[NamespaceElement]
)

trait NamespaceElement

case class Namespace(
  name: String,
  fullName: String,
  elements: Seq[NamespaceElement]
) extends NamespaceElement