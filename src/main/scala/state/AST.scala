package state

import state.api.{Core, TypeReference}

case class AttributeDefinition(
  name: String,
  typeReference: String
) {
  lazy val typeDefinition: ClassDefinition = {
    TypeReference.findType(typeReference) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"Unknown type $typeReference")
    }
  }
}

case class ParameterDefinition(
  name: String,
  typeReference: String
) {
  lazy val typeDefinition: ClassDefinition = {
    TypeReference.findType(typeReference) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"Unknown type $typeReference")
    }
  }
}

sealed trait ClassDefinition {
  def name: String

  def attributes: Seq[AttributeDefinition]

  def methods: Seq[MethodDefinition]
}

sealed trait MethodDefinition {
  def name: String

  def parameters: Seq[ParameterDefinition]

  def returnType: ClassDefinition
}

case class NativeClassDefinition(
  name: String,
  attributes: Seq[AttributeDefinition],
  methods: Seq[NativeMethodDefinition]
) extends ClassDefinition

case class NativeMethodDefinition(
  name: String,
  parameters: Seq[ParameterDefinition],
  returnTypeReference: String
) extends MethodDefinition  {
  lazy val returnType: ClassDefinition = {
    TypeReference.findType(returnTypeReference) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"Unknown type $returnTypeReference")
    }
  }
}

case class DefinedClassDefinition(
  name: String,
  attributes: Seq[AttributeDefinition],
  methods: Seq[DefinedMethodDefinition]
) extends ClassDefinition

case class DefinedMethodDefinition(
  name: String,
  function: DefinedFunction
) extends MethodDefinition {
  override def parameters: Seq[ParameterDefinition] = function.parameters

  override def returnType: ClassDefinition = function.returnType

  def body: Expression = function.body
}

sealed trait Expression {
  def returnType: ClassDefinition
}

sealed trait LogicalExpression extends Expression {
  override def returnType: ClassDefinition = Core.boolean
}

case class Or(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

case class And(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

case class Equal(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

case class NotEqual(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

case class Lower(left: Expression, right: Expression) extends LogicalExpression

case class Upper(left: Expression, right: Expression) extends LogicalExpression

case class LowerOrEqual(left: Expression, right: Expression) extends LogicalExpression

case class UpperOrEqual(left: Expression, right: Expression) extends LogicalExpression

case class Plus(left: Expression, right: Expression) extends LogicalExpression

case class Minus(left: Expression, right: Expression) extends LogicalExpression

case class Modulo(left: Expression, right: Expression) extends LogicalExpression

case class Time(left: Expression, right: Expression) extends LogicalExpression

case class Divide(left: Expression, right: Expression) extends LogicalExpression

case class Not(inner: LogicalExpression) extends LogicalExpression

case class BooleanValue(value: Boolean) extends LogicalExpression

case class NumberValue(value: BigDecimal) extends Expression {
  override def returnType: ClassDefinition = Core.number
}

case class QuotedStringValue(value: String) extends Expression {
  override def returnType: ClassDefinition = Core.string
}

case class Variable(name: String, typeReference: String) extends Expression {
  lazy val returnType: ClassDefinition = {
    TypeReference.findType(typeReference) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"Unknown type $typeReference")
    }
  }
}

case class MethodCall(expression: Expression, method: String, parameters: ListExpressionToken) extends Expression {
  lazy val returnType: ClassDefinition = expression.returnType.methods.find(_.name == method) match {
    case Some(methodDefinition) =>
      methodDefinition.returnType
    case None =>
      throw new RuntimeException(s"The type ${expression.returnType.name} does not have method $method")
  }
}

case class AttributeCall(expression: Expression, attribute: String) extends Expression {
  lazy val returnType: ClassDefinition = expression.returnType.attributes.find(_.name == attribute) match {
    case Some(attributeDefinition) =>
      attributeDefinition.typeDefinition
    case None =>
      throw new RuntimeException(s"The type ${expression.returnType.name} does not have attribute $attribute")
  }
}

case class CombinedExpression(parts: Seq[Expression]) extends Expression {
  lazy val returnType: ClassDefinition = parts.lastOption match {
    case Some(lastPart) => lastPart.returnType
    case None => Core.unit
  }
}

case class Condition(
  condition: Expression,
  onTrue: Expression,
  onFalse: Option[Expression]
) extends Expression {
  lazy val returnType: ClassDefinition = onFalse match {
    case None => Core.unit
    case Some(onFalseBody) if onTrue.returnType == onFalseBody.returnType => onTrue.returnType
    case _ => Core.any
  }
}

case class DefinedFunction(parameters: Seq[ParameterDefinition], body: Expression) {
  lazy val returnType: ClassDefinition = body.returnType
}

case class Parameter(name: String, typeReference: String) {
  lazy val typeDefinition: ClassDefinition = {
    TypeReference.findType(typeReference) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"Unknown type $typeReference")
    }
  }
}

case class Verification(name: String, message: String, function: DefinedFunction, comment: Option[String])

sealed trait Type extends ClassDefinition {
  def comment: Option[String]
}

case class DefinedType(name: String, attributes: Seq[AttributeDefinition], verifications: Seq[TypeVerification], inherited: Seq[String], comment: Option[String]) extends Type {
  lazy val inheritedVerifications: Seq[Verification] = inherited map { verificationReference =>
    TypeReference.findVerification(verificationReference) match {
      case Some(verification) => verification
      case None => throw new RuntimeException(s"Unknown verification $verificationReference")
    }
  }

  override def methods: Seq[MethodDefinition] = Seq()
}

case class AliasType(name: String, alias: String, inherited: Seq[String], comment: Option[String]) extends Type {
  lazy val typeAlias: ClassDefinition = {
    TypeReference.findType(alias) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"Unknown type $typeAlias")
    }
  }

  lazy val inheritedVerifications: Seq[Verification] = inherited map { verificationReference =>
    TypeReference.findVerification(verificationReference) match {
      case Some(verification) => verification
      case None => throw new RuntimeException(s"Unknown verification $verificationReference")
    }
  }

  override def attributes: Seq[AttributeDefinition] = typeAlias.attributes

  override def methods: Seq[MethodDefinition] = typeAlias.methods
}

case class TypeVerification(message: String, function: DefinedFunction)