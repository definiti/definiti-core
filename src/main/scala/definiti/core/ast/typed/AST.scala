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

private[core] case class TypedVerification(name: String, packageName: String, message: String, function: DefinedFunction, comment: Option[String], range: Range) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] sealed trait Type extends TypedClassDefinition {
  def comment: Option[String]
}

private[core] case class TypedDefinedType(name: String, packageName: String, genericTypes: Seq[String], attributes: Seq[AttributeDefinition], verifications: Seq[TypeVerification], inherited: Seq[VerificationReference], comment: Option[String], range: Range) extends Type {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class TypedAliasType(name: String, packageName: String, genericTypes: Seq[String], alias: TypeReference, inherited: Seq[VerificationReference], comment: Option[String], range: Range) extends Type {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class TypedNamedFunction(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  body: ast.Expression,
  range: Range
)