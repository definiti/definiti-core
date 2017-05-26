package definiti.core

case class HttpAST(
  requirements: Seq[Requirement],
  requests: Seq[Request],
  range: Range
)

case class Requirement(
  name: String,
  packageName: String,
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  comment: Option[String],
  range: Range
) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

case class Request(
  name: String,
  packageName: String,
  input: RequestInput,
  requiring: Seq[RequestRequirement],
  returning: Seq[RequestResult],
  comment: Option[String],
  range: Range
) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

case class RequestInput(
  method: String,
  requestUri: RequestUri,
  inputType: Option[TypeReference],
  range: Range
)

case class RequestUri(
  parts: Seq[RequestUriPart],
  query: Seq[ParameterDefinition],
  range: Range
)

sealed trait RequestUriPart {
  def range: Range
}

case class FixedPart(
  text: String,
  range: Range
) extends RequestUriPart

case class VariablePart(
  parameterDefinition: ParameterDefinition,
  range: Range
) extends RequestUriPart

case class RequestRequirement(
  requirementReference: RequirementReference,
  returning: RequestResult,
  range: Range
)

case class RequirementReference(
  name: String,
  parameters: Seq[String],
  range: Range
)

case class RequestResult(
  status: BigDecimal,
  output: Option[RequestOutput],
  range: Range
)

sealed trait RequestOutput {
  def range: Range
}

case class RawOutput(
  text: String,
  range: Range
) extends RequestOutput

case class ReferenceOutput(
  typeReference: TypeReference,
  range: Range
) extends RequestOutput