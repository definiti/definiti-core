package definiti.core

import definiti.core.parser.ImportsMap

case class Position(line: Long, column: Long) {
  def prettyPrint: String = {
    s"""$line-$column"""
  }
}

case class Range(start: Position, end: Position) {
  def prettyPrint: String = {
    s"""[${start.prettyPrint}, ${end.prettyPrint}]"""
  }
}

case class Root(
  files: Seq[RootFile]
)

case class RootFile(
  packageName: String,
  imports: ImportsMap,
  verifications: Seq[Verification],
  classDefinitions: Seq[ClassDefinition],
  namedFunctions: Seq[NamedFunction]
)

sealed trait AbstractTypeReference

case class TypeReference(
  typeName: String,
  genericTypes: Seq[TypeReference]
) extends AbstractTypeReference {
  def readableString: String = s"$typeName[${genericTypes.map(_.readableString).mkString(",")}]"
}

case class LambdaReference(
  inputTypes: Seq[TypeReference],
  outputType: TypeReference
) extends AbstractTypeReference {
  def readableString: String = s"(${inputTypes.map(_.readableString).mkString(", ")}) => ${outputType.readableString}"
}

case class AttributeDefinition(
  name: String,
  typeReference: TypeReference,
  comment: Option[String],
  verifications: Seq[VerificationReference],
  range: Range
)

case class ParameterDefinition(
  name: String,
  typeReference: AbstractTypeReference,
  range: Range
)

sealed trait ClassDefinition {
  def name: String

  def canonicalName: String

  def genericTypes: Seq[String]
}

sealed trait MethodDefinition {
  def name: String
  def genericTypes: Seq[String]
  def parameters: Seq[ParameterDefinition]
  def comment: Option[String]
}

case class NativeClassDefinition(
  name: String,
  genericTypes: Seq[String],
  attributes: Seq[AttributeDefinition],
  methods: Seq[NativeMethodDefinition],
  comment: Option[String]
) extends ClassDefinition {
  override def canonicalName: String = name
}

case class NativeMethodDefinition(
  name: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  returnTypeReference: TypeReference,
  comment: Option[String]
) extends MethodDefinition

case class DefinedMethodDefinition(
  name: String,
  genericTypes: Seq[String],
  function: DefinedFunction,
  comment: Option[String],
  range: Range
) extends MethodDefinition {
  def parameters: Seq[ParameterDefinition] = function.parameters

  def body: Expression = function.body
}

sealed trait Expression {
  def range: Range
}

sealed trait LogicalExpression extends Expression

sealed trait CalculatorExpression extends Expression

case class Or(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class And(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Equal(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class NotEqual(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Lower(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Upper(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class LowerOrEqual(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class UpperOrEqual(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Plus(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Minus(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Modulo(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Time(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Divide(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Not(inner: Expression, range: Range) extends LogicalExpression

case class BooleanValue(value: Boolean, range: Range) extends LogicalExpression

case class NumberValue(value: BigDecimal, range: Range) extends Expression

case class QuotedStringValue(value: String, range: Range) extends Expression

case class Variable(name: String, typeReference: TypeReference, range: Range) extends Expression

case class MethodCall(expression: Expression, method: String, parameters: Seq[Expression], generics: Seq[TypeReference], range: Range) extends Expression

case class AttributeCall(expression: Expression, attribute: String, range: Range) extends Expression

case class CombinedExpression(parts: Seq[Expression], range: Range) extends Expression

case class Condition(
  condition: Expression,
  onTrue: Expression,
  onFalse: Option[Expression],
  range: Range
) extends Expression

case class LambdaExpression(
  parameterList: Seq[ParameterDefinition],
  expression: Expression,
  range: Range
) extends Expression

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

case class VerificationReference(verificationName: String, message: Option[String], range: Range)

case class NamedFunction(name: String, packageName: String, function: DefinedFunction, range: Range) {
  def canonicalName: String = ASTHelper.canonical(packageName, name)
}