package definiti.common.ast

case class ExtendedContext[A](
  name: String,
  content: A,
  innerLocation: Location,
  location: Location
) extends NamespaceElement
