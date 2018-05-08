package definiti.common.ast

sealed trait ClassDefinition extends NamespaceElement {
  def name: String

  def fullName: String

  def genericTypes: Seq[String]
}

sealed trait ProjectClassDefinition extends ClassDefinition {
  def comment: Option[String]

  def location: Location
}

case class DefinedType(
  name: String,
  fullName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  attributes: Seq[AttributeDefinition],
  verifications: Seq[TypeVerification],
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location
) extends ProjectClassDefinition

case class AliasType(
  name: String,
  fullName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  alias: TypeDeclaration,
  inherited: Seq[VerificationReference],
  verifications: Seq[TypeVerification],
  comment: Option[String],
  location: Location
) extends ProjectClassDefinition

sealed trait TypeVerification {
  def message: VerificationMessage

  def function: DefinedFunction

  def location: Location
}

case class AtomicTypeVerification(
  message: VerificationMessage,
  function: DefinedFunction,
  location: Location
) extends TypeVerification

case class DependentTypeVerification(
  name: String,
  message: VerificationMessage,
  function: DefinedFunction,
  location: Location
) extends TypeVerification

case class Enum(
  name: String,
  fullName: String,
  cases: Seq[EnumCase],
  comment: Option[String],
  location: Location
) extends ProjectClassDefinition {
  override def genericTypes: Seq[String] = Seq.empty
}


case class NativeClassDefinition(
  name: String,
  fullName: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  methods: Seq[MethodDefinition],
  comment: Option[String]
) extends ClassDefinition

case class EnumCase(
  name: String,
  comment: Option[String],
  location: Location
)

case class AttributeDefinition(
  name: String,
  typeDeclaration: TypeDeclaration,
  comment: Option[String],
  verifications: Seq[VerificationReference],
  location: Location
)

case class MethodDefinition(
  name: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  comment: Option[String]
)

case class VerificationReference(
  verificationName: String,
  parameters: Seq[AtomicExpression],
  location: Location
)