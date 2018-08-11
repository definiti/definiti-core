package definiti.core.plugin.serialization

import definiti.common.ast._
import spray.json._

private[core] trait RootJsonSerialization {
  self: JsonSerialization =>

  def rootToJson(root: Root): String = {
    rootFormat.write(root).compactPrint
  }

  def rootFromJson(json: String): Root = {
    rootFormat.read(json.parseJson)
  }

  import spray.json.DefaultJsonProtocol._

  implicit lazy val rootFormat: JsonFormat[Root] = lazyFormat(jsonFormat1(Root.apply))
  implicit lazy val namespaceElementFormat: JsonFormat[NamespaceElement] = lazyFormat(sealedTraitFormat[NamespaceElement](
    Format("aliasType", classOf[AliasType]),
    Format("definedType", classOf[DefinedType]),
    Format("nativeClassDefinition", classOf[NativeClassDefinition]),
    Format("namedFunction", classOf[NamedFunction]),
    Format("verification", classOf[Verification])
  ))
  implicit lazy val namespaceFormat: JsonFormat[Namespace] = lazyFormat(jsonFormat3(Namespace.apply))
  implicit lazy val verificationFormat: JsonFormat[Verification] = lazyFormat(jsonFormat7(Verification.apply))
  implicit lazy val verificationMessageFormat: JsonFormat[VerificationMessage] = lazyFormat(sealedTraitFormat[VerificationMessage](
    Format("literal", classOf[LiteralMessage]),
    Format("typed", classOf[TypedMessage])
  ))
  implicit lazy val literalMessage: JsonFormat[LiteralMessage] = lazyFormat(jsonFormat2(LiteralMessage.apply))
  implicit lazy val typedMessage: JsonFormat[TypedMessage] = lazyFormat(jsonFormat3(TypedMessage.apply))
  implicit lazy val classDefinitionFormat: JsonFormat[ClassDefinition] = lazyFormat(sealedTraitFormat[ClassDefinition](
    Format("aliasType", classOf[AliasType]),
    Format("definedType", classOf[DefinedType]),
    Format("nativeClassDefinition", classOf[NativeClassDefinition])
  ))
  implicit lazy val definedTypeFormat: JsonFormat[DefinedType] = lazyFormat(jsonFormat9(DefinedType.apply))
  implicit lazy val aliasTypeFormat: JsonFormat[AliasType] = lazyFormat(jsonFormat9(AliasType.apply))
  implicit lazy val nativeClassDefinitionFormat: JsonFormat[NativeClassDefinition] = lazyFormat(jsonFormat6(NativeClassDefinition.apply))
  implicit lazy val namedFunctionFormat: JsonFormat[NamedFunction] = lazyFormat(jsonFormat7(NamedFunction.apply))

  implicit lazy val typeDeclarationFormat: JsonFormat[TypeDeclaration] = lazyFormat(jsonFormat4(TypeDeclaration.apply))
  implicit lazy val abstractTypeReferenceFormat: JsonFormat[AbstractTypeReference] = lazyFormat(sealedTraitFormat[AbstractTypeReference](
    Format("typeReference", classOf[TypeReference]),
    Format("lambdaReference", classOf[LambdaReference]),
    Format("namedFunctionReference", classOf[NamedFunctionReference])
  ))
  implicit lazy val typeReferenceFormat: JsonFormat[TypeReference] = lazyFormat(jsonFormat2(TypeReference.apply))
  implicit lazy val lambdaReferenceFormat: JsonFormat[LambdaReference] = lazyFormat(jsonFormat2(LambdaReference.apply))
  implicit lazy val namedFunctionReferenceFormat: JsonFormat[NamedFunctionReference] = lazyFormat(jsonFormat1(NamedFunctionReference.apply))
  implicit lazy val attributeDefinitionFormat: JsonFormat[AttributeDefinition] = lazyFormat(jsonFormat6(AttributeDefinition.apply))
  implicit lazy val parameterDefinitionFormat: JsonFormat[ParameterDefinition] = lazyFormat(jsonFormat3(ParameterDefinition.apply))
  implicit lazy val methodDefinitionFormat: JsonFormat[MethodDefinition] = lazyFormat(jsonFormat5(MethodDefinition.apply))
  implicit lazy val verificationReferenceFormat: JsonFormat[VerificationReference] = lazyFormat(jsonFormat3(VerificationReference.apply))
  implicit lazy val typeVerificationFormat: JsonFormat[TypeVerification] = lazyFormat(sealedTraitFormat[TypeVerification](
    Format("atomic", classOf[AtomicTypeVerification]),
    Format("dependent", classOf[DependentTypeVerification])
  ))
  implicit lazy val atomicTypeVerificationFormat: JsonFormat[AtomicTypeVerification] = lazyFormat(jsonFormat3(AtomicTypeVerification.apply))
  implicit lazy val dependentTypeVerificationFormat: JsonFormat[DependentTypeVerification] = lazyFormat(jsonFormat4(DependentTypeVerification.apply))
  implicit lazy val definedFunctionFormat: JsonFormat[DefinedFunction] = lazyFormat(jsonFormat4(DefinedFunction.apply))

  implicit lazy val expressionFormat: JsonFormat[Expression] = lazyFormat(sealedTraitFormat(
    Format("logicalExpression", classOf[LogicalExpression]),
    Format("not", classOf[Not]),
    Format("booleanValue", classOf[BooleanValue]),
    Format("numberValue", classOf[NumberValue]),
    Format("quotedStringValue", classOf[QuotedStringValue]),
    Format("reference", classOf[Reference]),
    Format("methodCall", classOf[MethodCall]),
    Format("attributeCall", classOf[AttributeCall]),
    Format("combinedExpression", classOf[CombinedExpression]),
    Format("condition", classOf[Condition]),
    Format("lambdaExpression", classOf[LambdaExpression]),
    Format("functionCall", classOf[FunctionCall])
  ))
  implicit lazy val logicalExpressionFormat: JsonFormat[LogicalExpression] = lazyFormat(jsonFormat5(LogicalExpression.apply))
  implicit lazy val notFormat: JsonFormat[Not] = lazyFormat(jsonFormat3(Not.apply))
  implicit lazy val atomicExpressionFormat: JsonFormat[AtomicExpression] = lazyFormat(sealedTraitFormat(
    Format("booleanValue", classOf[BooleanValue]),
    Format("numberValue", classOf[NumberValue]),
    Format("quotedStringValue", classOf[QuotedStringValue]),
    Format("reference", classOf[Reference])
  ))
  implicit lazy val booleanValueFormat: JsonFormat[BooleanValue] = lazyFormat(jsonFormat3(BooleanValue.apply))
  implicit lazy val numberValueFormat: JsonFormat[NumberValue] = lazyFormat(jsonFormat3(NumberValue.apply))
  implicit lazy val quotedStringValueFormat: JsonFormat[QuotedStringValue] = lazyFormat(jsonFormat3(QuotedStringValue.apply))
  implicit lazy val referenceFormat: JsonFormat[Reference] = lazyFormat(jsonFormat3(Reference.apply))
  implicit lazy val methodCallFormat: JsonFormat[MethodCall] = lazyFormat(jsonFormat6(MethodCall.apply))
  implicit lazy val attributeCallFormat: JsonFormat[AttributeCall] = lazyFormat(jsonFormat4(AttributeCall.apply))
  implicit lazy val combinedExpressionFormat: JsonFormat[CombinedExpression] = lazyFormat(jsonFormat3(CombinedExpression.apply))
  implicit lazy val conditionFormat: JsonFormat[Condition] = lazyFormat(jsonFormat5(Condition.apply))
  implicit lazy val lambdaExpressionFormat: JsonFormat[LambdaExpression] = lazyFormat(jsonFormat4(LambdaExpression.apply))
  implicit lazy val functionCallFormat: JsonFormat[FunctionCall] = lazyFormat(jsonFormat5(FunctionCall.apply))
  implicit lazy val logicalOperatorFormat: JsonFormat[LogicalOperator.Value] = lazyFormat(enumerationFormat(LogicalOperator))
  implicit lazy val calculatorOperatorFormat: JsonFormat[CalculatorOperator.Value] = lazyFormat(enumerationFormat(CalculatorOperator))
  implicit lazy val locationFormat: JsonFormat[Location] = lazyFormat(jsonFormat2(Location.apply))
  implicit lazy val rangeFormat: JsonFormat[Range] = lazyFormat(jsonFormat2(Range.apply))
  implicit lazy val positionFormat: JsonFormat[Position] = lazyFormat(jsonFormat2(Position.apply))
}
