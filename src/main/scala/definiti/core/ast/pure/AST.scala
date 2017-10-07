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

private[core] case class PureDefinedFunction(parameters: Seq[ParameterDefinition], body: PureExpression, genericTypes: Seq[String], range: Range)

private[core] case class PureParameter(name: String, typeReference: TypeReference, range: Range)

private[core] case class PureVerification(name: String, packageName: String, message: String, function: PureDefinedFunction, comment: Option[String], range: Range) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] sealed trait PureType extends PureClassDefinition {
  def comment: Option[String]
}

private[core] case class PureDefinedType(name: String, packageName: String, genericTypes: Seq[String], attributes: Seq[AttributeDefinition], verifications: Seq[PureTypeVerification], inherited: Seq[VerificationReference], comment: Option[String], range: Range) extends PureType {
  def methods: Seq[MethodDefinition] = Seq()

  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class PureAliasType(name: String, packageName: String, genericTypes: Seq[String], alias: TypeReference, inherited: Seq[VerificationReference], comment: Option[String], range: Range) extends PureType {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class PureTypeVerification(message: String, function: PureDefinedFunction, range: Range)

private[core] case class PureNamedFunction(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  body: PureExpression,
  range: Range
) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

private[core] case class PureExtendedContext[A](name: String, content: A, range: Range)