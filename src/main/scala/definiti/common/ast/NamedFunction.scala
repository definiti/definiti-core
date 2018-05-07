package definiti.common.ast

case class NamedFunction(
  name: String,
  fullName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  body: Expression,
  location: Location
) extends NamespaceElement
