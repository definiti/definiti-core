package definiti.core.plugin.serialization

import definiti.core.ContextPlugin
import definiti.core.ast.Location
import definiti.core.ast.pure._
import spray.json._

trait PureRootJsonSerialization {
  self: JsonSerialization =>

  def pureRootToJson(pureRoot: PureRoot): String = pureRootFormat.write(pureRoot).compactPrint

  def pureRootFromJson(json: String): PureRoot = pureRootFormat.read(json.parseJson)

  import spray.json.DefaultJsonProtocol._

  implicit lazy val pureRootFormat: JsonFormat[PureRoot] = lazyFormat(jsonFormat1(PureRoot.apply))
  implicit lazy val pureRootFileFormat: JsonFormat[PureRootFile] = lazyFormat(jsonFormat6(PureRootFile.apply))
  implicit lazy val pureClassDefinitionFormat: JsonFormat[PureClassDefinition] = lazyFormat(sealedTraitFormat[PureClassDefinition](
    Format("aliasType", classOf[PureAliasType]),
    Format("definedType", classOf[PureDefinedType]),
    Format("nativeClassDefinition", classOf[PureNativeClassDefinition])
  ))
  implicit lazy val pureNativeClassDefinitionFormat: JsonFormat[PureNativeClassDefinition] = lazyFormat(jsonFormat5(PureNativeClassDefinition.apply))
  implicit lazy val pureDefinedFunctionFormat: JsonFormat[PureDefinedFunction] = lazyFormat(jsonFormat4(PureDefinedFunction.apply))
  implicit lazy val pureVerificationFormat: JsonFormat[PureVerification] = lazyFormat(jsonFormat7(PureVerification.apply))
  implicit lazy val pureTypeFormat: JsonFormat[PureType] = lazyFormat(sealedTraitFormat[PureType](
    Format("aliasType", classOf[PureAliasType]),
    Format("definedType", classOf[PureDefinedType])
  ))
  implicit lazy val pureDefinedTypeFormat: JsonFormat[PureDefinedType] = lazyFormat(jsonFormat9(PureDefinedType.apply))
  implicit lazy val pureVerificationReferenceFormat: JsonFormat[PureVerificationReference] = lazyFormat(jsonFormat3(PureVerificationReference.apply))
  implicit lazy val pureAttributeDefinitionFormat: JsonFormat[PureAttributeDefinition] = lazyFormat(jsonFormat5(PureAttributeDefinition.apply))
  implicit lazy val pureAliasTypeFormat: JsonFormat[PureAliasType] = lazyFormat(jsonFormat9(PureAliasType.apply))
  implicit lazy val pureTypeVerificationFormat: JsonFormat[PureTypeVerification] = lazyFormat(jsonFormat3(PureTypeVerification.apply))
  implicit lazy val pureEnumFormat: JsonFormat[PureEnum] = lazyFormat(jsonFormat5(PureEnum.apply))
  implicit lazy val pureEnumCaseFormat: JsonFormat[PureEnumCase] = lazyFormat(jsonFormat3(PureEnumCase.apply))
  implicit lazy val pureNamedFunctionFormat: JsonFormat[PureNamedFunction] = lazyFormat(jsonFormat7(PureNamedFunction.apply))
  implicit lazy val pureExtendedContextFormat: JsonFormat[PureExtendedContext[_]] = lazyFormat(new JsonFormat[PureExtendedContext[_]] {
    override def write(obj: PureExtendedContext[_]): JsValue = {
      val contextPlugin = config.contexts
        .find(_.contextName == obj.name)
        .getOrElse(deserializationError(s"Unknown context ${obj.name}"))
        .asInstanceOf[ContextPlugin[Any]]
      JsObject(
        "name" -> obj.name.toJson,
        "content" -> contextPlugin.contextToJson(obj.content).parseJson,
        "location" -> obj.location.toJson
      )
    }

    override def read(json: JsValue): PureExtendedContext[_] = {
      json match {
        case JsObject(fields) =>
          val name = fields.get("name").map(_.convertTo[String]).getOrElse(deserializationError("Undefined field name"))
          val contextPlugin = config.contexts
            .find(_.contextName == name)
            .getOrElse(deserializationError(s"Unknown context ${name}"))
          PureExtendedContext(
            name = name,
            content = fields.get("content").map(content => contextPlugin.contextFromJson(content.compactPrint)).getOrElse(deserializationError("Undefined field content")),
            location = fields.get("location").map(_.convertTo[Location]).getOrElse(deserializationError("Undefined field location"))
          )
        case _ => deserializationError(s"Object expected, got: ${json}")
      }
    }
  })

  implicit lazy val pureExpressionFormat: JsonFormat[PureExpression] = lazyFormat(sealedTraitFormat[PureExpression](
    Format("logicalExpression", classOf[PureLogicalExpression]),
    Format("calculatorExpression", classOf[PureCalculatorExpression]),
    Format("not", classOf[PureNot]),
    Format("booleanValue", classOf[PureBooleanValue]),
    Format("numberValue", classOf[PureNumberValue]),
    Format("quotedStringValue", classOf[PureQuotedStringValue]),
    Format("reference", classOf[PureReference]),
    Format("methodCall", classOf[PureMethodCall]),
    Format("attributeCall", classOf[PureAttributeCall]),
    Format("combinedExpression", classOf[PureCombinedExpression]),
    Format("condition", classOf[PureCondition]),
    Format("lambdaExpression", classOf[PureLambdaExpression]),
    Format("functionCall", classOf[PureFunctionCall])
  ))
  implicit lazy val pureLogicalExpressionFormat: JsonFormat[PureLogicalExpression] = lazyFormat(jsonFormat4(PureLogicalExpression.apply))
  implicit lazy val pureCalculatorExpressionFormat: JsonFormat[PureCalculatorExpression] = lazyFormat(jsonFormat4(PureCalculatorExpression.apply))
  implicit lazy val pureNotFormat: JsonFormat[PureNot] = lazyFormat(jsonFormat2(PureNot.apply))
  implicit lazy val pureAtomicExpressionFormat: JsonFormat[PureAtomicExpression] = lazyFormat(sealedTraitFormat[PureAtomicExpression](
    Format("booleanValue", classOf[PureBooleanValue]),
    Format("numberValue", classOf[PureNumberValue]),
    Format("quotedStringValue", classOf[PureQuotedStringValue]),
    Format("reference", classOf[PureReference])
  ))
  implicit lazy val pureBooleanValueFormat: JsonFormat[PureBooleanValue] = lazyFormat(jsonFormat2(PureBooleanValue.apply))
  implicit lazy val pureNumberValueFormat: JsonFormat[PureNumberValue] = lazyFormat(jsonFormat2(PureNumberValue.apply))
  implicit lazy val pureQuotedStringValueFormat: JsonFormat[PureQuotedStringValue] = lazyFormat(jsonFormat2(PureQuotedStringValue.apply))
  implicit lazy val pureReferenceFormat: JsonFormat[PureReference] = lazyFormat(jsonFormat2(PureReference.apply))
  implicit lazy val pureMethodCallFormat: JsonFormat[PureMethodCall] = lazyFormat(jsonFormat5(PureMethodCall.apply))
  implicit lazy val pureAttributeCallFormat: JsonFormat[PureAttributeCall] = lazyFormat(jsonFormat3(PureAttributeCall.apply))
  implicit lazy val pureCombinedExpressionFormat: JsonFormat[PureCombinedExpression] = lazyFormat(jsonFormat2(PureCombinedExpression.apply))
  implicit lazy val pureConditionFormat: JsonFormat[PureCondition] = lazyFormat(jsonFormat4(PureCondition.apply))
  implicit lazy val pureLambdaExpressionFormat: JsonFormat[PureLambdaExpression] = lazyFormat(jsonFormat3(PureLambdaExpression.apply))
  implicit lazy val pureFunctionCallFormat: JsonFormat[PureFunctionCall] = lazyFormat(jsonFormat4(PureFunctionCall.apply))
}
