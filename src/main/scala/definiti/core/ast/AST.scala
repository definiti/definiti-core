package definiti.core.ast

case class Root(
  elements: Seq[NamespaceElement]
)

sealed trait NamespaceElement

case class Namespace(
  name: String,
  fullName: String,
  elements: Seq[NamespaceElement]
) extends NamespaceElement

case class Verification(
  name: String,
  message: String,
  function: DefinedFunction,
  comment: Option[String],
  location: Location
) extends NamespaceElement

sealed trait ClassDefinition extends NamespaceElement {
  def name: String

  def genericTypes: Seq[String]
}

case class DefinedType(
  name: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  verifications: Seq[TypeVerification],
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location
) extends ClassDefinition

case class AliasType(
  name: String,
  genericTypes: Seq[String],
  alias: TypeReference,
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location
) extends ClassDefinition

case class NativeClassDefinition(
  name: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  methods: Seq[MethodDefinition],
  comment: Option[String]
) extends ClassDefinition

case class Enum(
  name: String,
  cases: Seq[EnumCase],
  comment: Option[String],
  location: Location
) extends ClassDefinition {
  override def genericTypes: Seq[String] = Seq.empty
}

case class EnumCase(
  name: String,
  comment: Option[String],
  location: Location
)

case class NamedFunction(
  name: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  body: Expression,
  location: Location
) extends NamespaceElement

case class ExtendedContext[A](
  name: String,
  content: A,
  location: Location
) extends NamespaceElement

sealed trait AbstractTypeReference {
  def readableString: String
}

case class TypeReference(
  typeName: String,
  genericTypes: Seq[TypeReference] = Seq.empty
) extends AbstractTypeReference {
  def readableString: String = {
    if (genericTypes.nonEmpty) {
      s"$typeName[${genericTypes.map(_.readableString).mkString(",")}]"
    } else {
      typeName
    }
  }
}

case class LambdaReference(
  inputTypes: Seq[TypeReference],
  outputType: TypeReference
) extends AbstractTypeReference {
  def readableString: String = s"(${inputTypes.map(_.readableString).mkString(", ")}) => ${outputType.readableString}"
}

case class NamedFunctionReference(
  functionName: String
) extends AbstractTypeReference {
  def readableString: String = functionName
}

case class AttributeDefinition(
  name: String,
  typeReference: TypeReference,
  comment: Option[String],
  verifications: Seq[VerificationReference],
  location: Location
)

case class ParameterDefinition(
  name: String,
  typeReference: AbstractTypeReference,
  location: Location
)

case class MethodDefinition(
  name: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  comment: Option[String]
)

case class VerificationReference(verificationName: String, message: Option[String], location: Location)

case class TypeVerification(message: String, function: DefinedFunction, location: Location)

case class DefinedFunction(parameters: Seq[ParameterDefinition], body: Expression, genericTypes: Seq[String], location: Location)

case class Parameter(name: String, typeReference: TypeReference, location: Location)