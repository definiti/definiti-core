package definiti.core.ast.typed

import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.{ASTHelper, ast}

private[core] case class TypedRoot(
  files: Seq[TypedRootFile]
)

private[core] case class TypedRootFile(
  packageName: String,
  verifications: Seq[TypedVerification],
  classDefinitions: Seq[TypedClassDefinition],
  namedFunctions: Seq[TypedNamedFunction],
  contexts: Seq[PureExtendedContext[_]]
)

private[core] sealed trait TypedClassDefinition {
  def name: String

  def canonicalName: String

  def genericTypes: Seq[String]
}

private[core] case class TypedNativeClassDefinition(
  name: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  methods: Seq[MethodDefinition],
  comment: Option[String]
) extends TypedClassDefinition {
  override def canonicalName: String = name
}

private[core] case class TypedVerification(
  name: String,
  packageName: String,
  parameters: Seq[ParameterDefinition],
  message: VerificationMessage,
  function: DefinedFunction,
  comment: Option[String],
  location: Location
) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] sealed trait Type extends TypedClassDefinition {
  def comment: Option[String]
}

private[core] case class TypedDefinedType(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  attributes: Seq[AttributeDefinition],
  verifications: Seq[TypeVerification],
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location
) extends Type {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class TypedAliasType(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  alias: TypeDeclaration,
  verifications: Seq[TypeVerification],
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location
) extends Type {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class TypedEnum(
  name: String,
  packageName: String,
  cases: Seq[TypedEnumCase],
  comment: Option[String],
  location: Location
) extends Type {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)

  override def genericTypes: Seq[String] = Seq.empty
}

private[core] case class TypedEnumCase(
  name: String,
  comment: Option[String],
  location: Location
)

private[core] case class TypedNamedFunction(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  body: ast.Expression,
  location: Location
)