package definiti.common.ast

case class ExtendedContext[A](
  name: String,
  content: A,
  location: Location
) extends NamespaceElement
