package definiti.core.ast.typed

import definiti.core.ASTHelper
import definiti.core.ast.Range
import definiti.core.ast.pure._

case class Root(
  files: Seq[RootFile]
)

case class RootFile(
  packageName: String,
  verifications: Seq[Verification],
  classDefinitions: Seq[ClassDefinition],
  namedFunctions: Seq[NamedFunction],
  contexts: Seq[ExtendedContext[_]]
)

sealed trait ClassDefinition {
  def name: String

  def canonicalName: String

  def genericTypes: Seq[String]
}

case class NativeClassDefinition(
  name: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  methods: Seq[MethodDefinition],
  comment: Option[String]
) extends ClassDefinition {
  override def canonicalName: String = name
}

case class DefinedFunction(parameters: Seq[ParameterDefinition], body: Expression, genericTypes: Seq[String], range: Range)

case class Parameter(name: String, typeReference: TypeReference, range: Range)

case class Verification(name: String, packageName: String, message: String, function: DefinedFunction, comment: Option[String], range: Range) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}

sealed trait Type extends ClassDefinition {
  def comment: Option[String]
}

case class DefinedType(name: String, packageName: String, genericTypes: Seq[String], attributes: Seq[AttributeDefinition], verifications: Seq[TypeVerification], inherited: Seq[VerificationReference], comment: Option[String], range: Range) extends Type {
  def methods: Seq[MethodDefinition] = Seq()

  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

case class AliasType(name: String, packageName: String, genericTypes: Seq[String], alias: TypeReference, inherited: Seq[VerificationReference], comment: Option[String], range: Range) extends Type {
  override def canonicalName: String = ASTHelper.canonical(packageName, name)
}

case class TypeVerification(message: String, function: DefinedFunction, range: Range)

case class NamedFunction(
  name: String,
  packageName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnType: TypeReference,
  body: Expression,
  range: Range
)