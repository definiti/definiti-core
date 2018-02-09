package definiti.core.ast.pure

import definiti.core.ASTHelper
import definiti.core.ast._

private[core] case class PureRoot(
  files: Seq[PureRootFile]
)

private[core] case class PureRootFile(
  packageName: String,
  imports: ImportsMap,
  verifications: Seq[PureVerification],
  classDefinitions: Seq[PureClassDefinition],
  namedFunctions: Seq[PureNamedFunction],
  contexts: Seq[PureExtendedContext[_]]
)

private[core] sealed trait PureClassDefinition {
  def name: String

  def canonicalName: String

  def genericTypes: Seq[String]
}

private[core] case class PureNativeClassDefinition(
  name: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  methods: Seq[MethodDefinition],
  comment: Option[String]
) extends PureClassDefinition {
  override def canonicalName: String = name
}

private[core] case class PureDefinedFunction(parameters: Seq[ParameterDefinition], body: PureExpression, genericTypes: Seq[String], location: Location)

private[core] case class PureParameter(name: String, typeReference: TypeReference, location: Location)

private[core] case class PureVerification(name: String, packageName: String, message: VerificationMessage, function: PureDefinedFunction, comment: Option[String], location: Location) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] sealed trait PureType extends PureClassDefinition {
  def comment: Option[String]
}

private[core] case class PureDefinedType(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  verifications: Seq[PureTypeVerification],
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location
) extends PureType {
  def methods: Seq[MethodDefinition] = Seq()

  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class PureAliasType(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  alias: TypeReference,
  verifications: Seq[PureTypeVerification],
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location
) extends PureType {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class PureEnum(
  name: String,
  packageName: String,
  cases: Seq[PureEnumCase],
  comment: Option[String],
  location: Location
) extends PureClassDefinition {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)

  override def genericTypes: Seq[String] = Seq.empty
}

private[core] case class PureEnumCase(
  name: String,
  comment: Option[String],
  location: Location
)

private[core] case class PureTypeVerification(message: VerificationMessage, function: PureDefinedFunction, location: Location)

private[core] case class PureNamedFunction(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  body: PureExpression,
  location: Location
) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class PureExtendedContext[A](name: String, content: A, location: Location)