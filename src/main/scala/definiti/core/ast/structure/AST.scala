package definiti.core.ast.structure

import definiti.core.ast.{Range, pure, typed}

case class Root(
  elements: Seq[PackageElement]
)

sealed trait PackageElement

case class Package(
  name: String,
  elements: Seq[PackageElement]
) extends PackageElement

case class Verification(
  name: String,
  message: String,
  function: typed.DefinedFunction,
  comment: Option[String],
  range: Range
) extends PackageElement

sealed trait ClassDefinition extends PackageElement {
  def name: String

  def genericTypes: Seq[String]
}

case class DefinedType(
  name: String,
  genericTypes: Seq[String],
  attributes: Seq[pure.AttributeDefinition],
  verifications: Seq[typed.TypeVerification],
  inherited: Seq[pure.VerificationReference],
  comment: Option[String],
  range: Range
) extends ClassDefinition {
  def methods: Seq[pure.MethodDefinition] = Seq()
}

case class AliasType(
  name: String,
  genericTypes: Seq[String],
  alias: pure.TypeReference,
  inherited: Seq[pure.VerificationReference],
  comment: Option[String],
  range: Range
) extends ClassDefinition

case class NamedFunction(
  name: String,
  genericTypes: Seq[String],
  parameters: Seq[pure.ParameterDefinition],
  returnType: pure.TypeReference,
  body: typed.Expression,
  range: Range
) extends PackageElement

case class ExtendedContext[A](
  name: String,
  content: A,
  range: Range
) extends PackageElement