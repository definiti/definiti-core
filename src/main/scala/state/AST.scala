package state

import spray.json.{JsObject, JsString, JsValue, JsonFormat}
import state.api.{Core, TypeReference}

case class Root(
  verifications: Seq[Verification],
  classDefinitions: Seq[ClassDefinition]
)

case class AttributeDefinition(
  name: String,
  typeReference: String,
  comment: Option[String]
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

  def comment: Option[String]
}

sealed trait MethodDefinition {
  def name: String

  def parameters: Seq[ParameterDefinition]

  def returnType: ClassDefinition

  def comment: Option[String]
}

case class NativeClassDefinition(
  name: String,
  attributes: Seq[AttributeDefinition],
  methods: Seq[NativeMethodDefinition],
  comment: Option[String]
) extends ClassDefinition

case class NativeMethodDefinition(
  name: String,
  parameters: Seq[ParameterDefinition],
  returnTypeReference: String,
  comment: Option[String]
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
  methods: Seq[DefinedMethodDefinition],
  comment: Option[String]
) extends ClassDefinition

case class DefinedMethodDefinition(
  name: String,
  function: DefinedFunction,
  comment: Option[String]
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

case class Or(left: Expression, right: Expression) extends LogicalExpression

case class And(left: Expression, right: Expression) extends LogicalExpression

case class Equal(left: Expression, right: Expression) extends LogicalExpression

case class NotEqual(left: Expression, right: Expression) extends LogicalExpression

case class Lower(left: Expression, right: Expression) extends LogicalExpression

case class Upper(left: Expression, right: Expression) extends LogicalExpression

case class LowerOrEqual(left: Expression, right: Expression) extends LogicalExpression

case class UpperOrEqual(left: Expression, right: Expression) extends LogicalExpression

case class Plus(left: Expression, right: Expression) extends LogicalExpression

case class Minus(left: Expression, right: Expression) extends LogicalExpression

case class Modulo(left: Expression, right: Expression) extends LogicalExpression

case class Time(left: Expression, right: Expression) extends LogicalExpression

case class Divide(left: Expression, right: Expression) extends LogicalExpression

case class Not(inner: Expression) extends LogicalExpression

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

case class MethodCall(expression: Expression, method: String, parameters: Seq[Expression]) extends Expression {
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

object ASTJsonProtocol {
  import spray.json.DefaultJsonProtocol._

  implicit val orFormat: JsonFormat[Or] = jsonFormat2(Or.apply)
  implicit val andFormat: JsonFormat[And] = jsonFormat2(And.apply)
  implicit val equalFormat: JsonFormat[Equal] = jsonFormat2(Equal.apply)
  implicit val notEqualFormat: JsonFormat[NotEqual] = jsonFormat2(NotEqual.apply)
  implicit val lowerFormat: JsonFormat[Lower] = jsonFormat2(Lower.apply)
  implicit val upperFormat: JsonFormat[Upper] = jsonFormat2(Upper.apply)
  implicit val lowerOrEqualFormat: JsonFormat[LowerOrEqual] = jsonFormat2(LowerOrEqual.apply)
  implicit val upperOrEqualFormat: JsonFormat[UpperOrEqual] = jsonFormat2(UpperOrEqual.apply)
  implicit val plusFormat: JsonFormat[Plus] = jsonFormat2(Plus.apply)
  implicit val minusFormat: JsonFormat[Minus] = jsonFormat2(Minus.apply)
  implicit val moduloFormat: JsonFormat[Modulo] = jsonFormat2(Modulo.apply)
  implicit val timeFormat: JsonFormat[Time] = jsonFormat2(Time.apply)
  implicit val divideFormat: JsonFormat[Divide] = jsonFormat2(Divide.apply)
  implicit val notFormat: JsonFormat[Not] = jsonFormat1(Not.apply)
  implicit val booleanValueFormat: JsonFormat[BooleanValue] = jsonFormat1(BooleanValue.apply)
  implicit val numberValueFormat: JsonFormat[NumberValue] = jsonFormat1(NumberValue.apply)
  implicit val quotedStringValueFormat: JsonFormat[QuotedStringValue] = jsonFormat1(QuotedStringValue.apply)
  implicit val variableFormat: JsonFormat[Variable] = jsonFormat(Variable.apply, "name", "typeReference")
  implicit val methodCallFormat: JsonFormat[MethodCall] = jsonFormat(MethodCall.apply, "expression", "method", "parameters")
  implicit val attributeCallFormat: JsonFormat[AttributeCall] = jsonFormat(AttributeCall.apply, "expression", "attribute")
  implicit val combinedExpressionFormat: JsonFormat[CombinedExpression] = jsonFormat(CombinedExpression.apply _, "parts")
  implicit val conditionFormat: JsonFormat[Condition] = jsonFormat(Condition.apply, "condition", "onTrue", "onFalse")

  implicit def expressionFormat: JsonFormat[Expression] = new JsonFormat[Expression] {
    def jsObject(typeName: String, content: JsValue): JsValue = {
      JsObject(
        "type" -> JsString(typeName),
        "content" -> content
      )
    }

    override def read(json: JsValue): Expression = ???

    override def write(obj: Expression): JsValue = obj match {
      case exp: BooleanValue => jsObject("BooleanValue", booleanValueFormat.write(exp))
      case exp: NumberValue => jsObject("NumberValue", numberValueFormat.write(exp))
      case exp: QuotedStringValue => jsObject("QuotedStringValue", quotedStringValueFormat.write(exp))
      case exp: Variable => jsObject("Variable", variableFormat.write(exp))
      case exp: MethodCall => jsObject("MethodCall", methodCallFormat.write(exp))
      case exp: AttributeCall => jsObject("AttributeCall", attributeCallFormat.write(exp))
      case exp: CombinedExpression => jsObject("CombinedExpression", combinedExpressionFormat.write(exp))
      case exp: Condition => jsObject("Condition", conditionFormat.write(exp))
      case exp: Or => jsObject("Or", orFormat.write(exp))
      case exp: And => jsObject("And", andFormat.write(exp))
      case exp: Equal => jsObject("Equal", equalFormat.write(exp))
      case exp: NotEqual => jsObject("NotEqual", notEqualFormat.write(exp))
      case exp: Lower => jsObject("Lower", lowerFormat.write(exp))
      case exp: Upper => jsObject("Upper", upperFormat.write(exp))
      case exp: LowerOrEqual => jsObject("LowerOrEqual", lowerOrEqualFormat.write(exp))
      case exp: UpperOrEqual => jsObject("UpperOrEqual", upperOrEqualFormat.write(exp))
      case exp: Plus => jsObject("Plus", plusFormat.write(exp))
      case exp: Minus => jsObject("Minus", minusFormat.write(exp))
      case exp: Modulo => jsObject("Modulo", moduloFormat.write(exp))
      case exp: Time => jsObject("Time", timeFormat.write(exp))
      case exp: Divide => jsObject("Divide", divideFormat.write(exp))
      case exp: Not => jsObject("Not", notFormat.write(exp))
    }
  }

  implicit val parameterDefinitionFormat: JsonFormat[ParameterDefinition] = jsonFormat(ParameterDefinition.apply, "name", "typeReference")
  implicit val attributeDefinitionFormat: JsonFormat[AttributeDefinition] = jsonFormat(AttributeDefinition.apply, "name", "typeReference", "comment")
  implicit val parameterFormat: JsonFormat[Parameter] = jsonFormat(Parameter.apply, "name", "typeReference")
  implicit val definedFunctionFormat: JsonFormat[DefinedFunction] = jsonFormat(DefinedFunction.apply, "parameters", "body")
  implicit val verificationFormat: JsonFormat[Verification] = jsonFormat4(Verification.apply)
  implicit val typeVerificationFormat: JsonFormat[TypeVerification] = jsonFormat2(TypeVerification.apply)
  implicit val definedTypeFormat: JsonFormat[DefinedType] = jsonFormat(DefinedType.apply, "name", "attributes", "verifications", "inherited", "comment")
  implicit val aliasTypeFormat: JsonFormat[AliasType] = jsonFormat(AliasType.apply, "name", "alias", "inherited", "comment")
  implicit val nativeMethodDefinitionFormat: JsonFormat[NativeMethodDefinition] = jsonFormat(NativeMethodDefinition.apply, "name", "parameters", "returnTypeReference", "comment")
  implicit val nativeClassDefinitionFormat: JsonFormat[NativeClassDefinition] = jsonFormat(NativeClassDefinition.apply, "name", "attributes", "methods", "comment")
  implicit val definedMethodDefinitionFormat: JsonFormat[DefinedMethodDefinition] = jsonFormat(DefinedMethodDefinition.apply, "name", "function", "comment")
  implicit val definedClassDefinitionFormat: JsonFormat[DefinedClassDefinition] = jsonFormat(DefinedClassDefinition.apply, "name", "attributes", "methods", "comment")

  implicit val classDefinitionFormat: JsonFormat[ClassDefinition] = new JsonFormat[ClassDefinition] {
    override def read(json: JsValue): ClassDefinition = ???

    override def write(obj: ClassDefinition): JsValue = obj match {
      case aliasType: AliasType => aliasTypeFormat.write(aliasType)
      case definedType: DefinedType => definedTypeFormat.write(definedType)
    }
  }

  implicit val rootFormat: JsonFormat[Root] = jsonFormat2(Root.apply)
}