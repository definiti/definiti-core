package definiti.common.ast

case class Root(
  namespaces: Seq[Namespace]
)

case class Namespace(
  name: String,
  fullName: String,
  elements: Seq[NamespaceElement]
)

trait NamespaceElement