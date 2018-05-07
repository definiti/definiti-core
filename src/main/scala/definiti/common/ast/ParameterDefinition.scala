package definiti.common.ast

case class ParameterDefinition(
  name: String,
  typeReference: AbstractTypeReference,
  location: Location
)
