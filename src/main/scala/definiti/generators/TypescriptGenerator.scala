package definiti.generators

import definiti._
import definiti.api.{ASTHelper, Context}

import scala.io.Source

object TypescriptGenerator {
  val nativeTypeMapping = Map(
    "Date" -> "DateWrapper",
    "Number" -> "NumberWrapper",
    "String" -> "StringWrapper",
    "List" -> "ListWrapper"
  )

  def generate(root: Root)(implicit context: Context): String = {
    val buffer: StringBuilder = new StringBuilder()

    appendNative(buffer)

    buffer.append(
      s"""
         |${root.verifications.map(generateVerification).mkString("\n")}
         |
         |function verify(message: string, condition: () => Boolean): string|null {
         |  if (condition()) {
         |    return null;
         |  } else {
         |    return message;
         |  }
         |}
      """.stripMargin
    )

    buffer.append(
      s"""
         |${root.classDefinitions.map(generateClassDefinition).mkString("\n")}
       """.stripMargin
    )

    buffer.toString()
  }

  private def appendNative(buffer: StringBuilder)(implicit context: Context): Unit = {
    Seq("DateWrapper", "ListWrapper", "NumberWrapper", "StringWrapper") foreach { className =>
      buffer.append(Source.fromResource(s"generators/ts/native/$className.ts").getLines.mkString("", "\n", "\n"))
    }
  }

  private def generateVerification(verification: Verification)(implicit context: Context): String = {
    s"""
       |${verification.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |export function verify${verification.name}(${generateParameters(verification.function.parameters)}): string|null {
       |  return verify("${verification.message}", () => {
       |    return ${generateExpression(verification.function.body)};
       |  })
       |}
      """.stripMargin
  }

  private def generateParameters(parameterDefinitions: Seq[ParameterDefinition])(implicit context: Context): String = parameterDefinitions match {
    case Nil => ""
    case one :: Nil => generateParameter(one)
    case seq => seq.map(generateParameter).mkString("(", "), (", ")")
  }

  private def generateParameter(parameterDefinition: ParameterDefinition)(implicit context: Context): String = {
    val parameterType = context.findType(parameterDefinition.typeReference.typeName) match {
      case Some(_: Type) => "$" + parameterDefinition.typeReference.typeName
      case _ => parameterDefinition.typeReference.typeName
    }
    val parameterName = parameterDefinition.name
    val finalParameterType = nativeTypeMapping.getOrElse(parameterType, parameterType)
    val parameterGenerics = generateGenericTypes(parameterDefinition.genericTypes)
    s"$parameterName: $finalParameterType$parameterGenerics"
  }

  private def generateExpression(expression: Expression)(implicit context: Context): String = expression match {
    case BooleanValue(value, _) => s"${value.toString}"
    case NumberValue(value, _) => s"new NumberWrapper(${value.toString})"
    case QuotedStringValue(value, _) => """new StringWrapper("""" + value.toString.replaceAllLiterally("\\", "\\\\") + """")"""
    case Variable(variable, _, _) => variable
    case MethodCall(inner, method, parameters, _) =>
      s"(${generateExpression(inner)}).$method(${generateCallParameters(parameters)})"
    case AttributeCall(inner, attribute, _) =>
      s"(${generateExpression(inner)}).$attribute"
    case CombinedExpression(expressions, _) =>
      expressions.map(generateExpression).mkString("\n")
    case Condition(condition, onTrue, onFalse, _) =>
      onFalse match {
        case Some(onFalseBody) =>
          s""" (${generateExpression(condition)})
             |  ? (${generateExpression(onTrue)})
             |  : (${generateExpression(onFalseBody)})
             |""".stripMargin
        case None =>
          s""" (${generateExpression(condition)})
             |  ? (${generateExpression(onTrue)})
             |  : null
             |""".stripMargin
      }
    case Or(left, right, _) => logicalExpression("||", "or", left, right)
    case And(left, right, _) => logicalExpression("&&", "and", left, right)
    case Equal(left, right, _) => logicalExpression("==", "equals", left, right)
    case NotEqual(left, right, _) => logicalExpression("!=", "notEquals", left, right)
    case Lower(left, right, _) => logicalExpression("<", "lower", left, right)
    case Upper(left, right, _) => logicalExpression(">", "upper", left, right)
    case LowerOrEqual(left, right, _) => logicalExpression("<=", "lowerOrEquals", left, right)
    case UpperOrEqual(left, right, _) => logicalExpression(">=", "upperOrEquals", left, right)
    case Plus(left, right, _) => logicalExpression("+", "plus", left, right)
    case Minus(left, right, _) => logicalExpression("-", "minus", left, right)
    case Modulo(left, right, _) => logicalExpression("%", "modulo", left, right)
    case Time(left, right, _) => logicalExpression("*", "time", left, right)
    case Divide(left, right, _) => logicalExpression("/", "divide", left, right)
    case Not(inner, _) => s"!(${generateExpression(inner)})"
  }

  private def logicalExpression(symbol: String, wrapperMethod: String, left: Expression, right: Expression)(implicit context: Context): String = {
    if (nativeTypeMapping.contains(ASTHelper.getReturnTypeOfExpression(left).classDefinition.name)) {
      s"(${generateExpression(left)}).$wrapperMethod(${generateExpression(right)})"
    } else {
      s"(${generateExpression(left)}) $symbol (${generateExpression(right)})"
    }
  }

  private def generateCallParameters(expressions: Seq[Expression])(implicit context: Context): String = expressions match {
    case Nil => ""
    case one :: Nil => generateExpression(one)
    case seq => seq.map(generateExpression).mkString("(", "), (", ")")
  }

  private def generateClassDefinition(classDefinition: ClassDefinition)(implicit context: Context): String = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
  }

  private def generateDefinedType(definedType: DefinedType, originalTypeOpt: Option[TypeReference] = None)(implicit context: Context): String = {
    val __resultAliases = definedType.verifications
      .flatMap(_.function.parameters.map(_.name))
      .distinct
      .map(parameter => s"const $parameter = __result;")
      .mkString("\n")

    val verifications = (
      definedType.verifications.map(generateTypeVerification)
        ++
        definedType.inherited.map(inherited => s"verify$inherited(__result)")
      ).mkString(", ")

    val realType = originalTypeOpt.map(_.typeName).getOrElse(definedType.name)

    val $interface = originalTypeOpt.map(_ => "").getOrElse(generateInterface(definedType))

    val originalTypeGenerics = originalTypeOpt match {
      case Some(originalType) => generateGenericTypes(originalType.genericTypes)
      case None => generateGenericTypeDefinition(definedType)
    }

    val typeDefinition = s"${generateGenericTypeDefinition(definedType)}"
    s"""
       |${$interface}
       |${definedType.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |export interface ${definedType.name}$typeDefinition extends $$$realType$originalTypeGenerics {}
       |
       |export function ${definedType.name}$typeDefinition(${generateAttributeParameters(definedType.attributes)}): string|Readonly<${definedType.name}$typeDefinition> {
       |  const __result = {${definedType.attributes.map(_.name).mkString(", ")}};
       |  ${__resultAliases}
       |  let __errorOpt = null;
       |  const __verifications = [
       |    $verifications
       |  ];
       |  for (let __i = 0 ; __i < __verifications.length ; __i++) {
       |    if (__verifications[__i] !== null) {
       |      __errorOpt = __verifications[__i];
       |    }
       |  }
       |
       |  if (__errorOpt) {
       |    return __errorOpt;
       |  } else {
       |    return Object.freeze(__result);
       |  }
       |}
     """.stripMargin
  }

  private def generateInterface(definedType: DefinedType)(implicit context: Context): String = {
    s"""
       |interface $$${definedType.name}${generateGenericTypeDefinition(definedType)}  {
       |  ${definedType.attributes.map(attribute => generateAttributeParameter(attribute) + ";").mkString("\n")}
       |}
     """.stripMargin
  }

  private def generateGenericTypeDefinition(definedType: DefinedType) = {
    if (definedType.genericTypes.nonEmpty) {
      definedType.genericTypes.mkString("<", ",", ">")
    } else {
      ""
    }
  }

  private def generateAliasType(aliasType: AliasType)(implicit context: Context): String = {
    context.findType(aliasType.alias.typeName) match {
      case Some(definedType: DefinedType) =>
        val genericTypeMapping = Map(definedType.genericTypes.zip(aliasType.alias.genericTypes): _*)
        def updateGenericTypes(typeReference: TypeReference): TypeReference = {
          if (genericTypeMapping.contains(typeReference.typeName)) {
            genericTypeMapping(typeReference.typeName)
          } else {
            typeReference.copy(
              genericTypes = typeReference.genericTypes.map(updateGenericTypes)
            )
          }
        }
        generateDefinedType(definedType.copy(
          comment = aliasType.comment,
          name = aliasType.name,
          genericTypes = aliasType.genericTypes,
          inherited = definedType.inherited ++ aliasType.inherited,
          attributes = definedType.attributes.map { attribute =>
            attribute.copy(typeReference = updateGenericTypes(attribute.typeReference))
          }
        ), Some(aliasType.alias))
      case _ => throw new RuntimeException("Undefined type: " + aliasType)
    }
  }

  private def generateAttributes(attributeDefinition: Seq[AttributeDefinition])(implicit context: Context): String = {
    attributeDefinition.map(generateAttribute).mkString(", ")
  }

  private def generateAttribute(attributeDefinition: AttributeDefinition)(implicit context: Context): String = {
    s"val ${attributeDefinition.name}: ${nativeTypeMapping.getOrElse(attributeDefinition.typeReference.typeName, attributeDefinition.typeReference)}"
  }

  private def generateAttributeParameters(attributeDefinition: Seq[AttributeDefinition])(implicit context: Context): String = {
    attributeDefinition.map(generateAttributeParameter).mkString(", ")
  }

  private def generateAttributeParameter(attributeDefinition: AttributeDefinition)(implicit context: Context): String = {
    val attributeName = attributeDefinition.name
    val attributeType = nativeTypeMapping.getOrElse(attributeDefinition.typeReference.typeName, attributeDefinition.typeReference.typeName)
    val attributeGenerics = generateGenericTypes(attributeDefinition.typeReference.genericTypes)
    s"$attributeName: $attributeType$attributeGenerics"
  }

  private def generateTypeVerification(typeVerification: TypeVerification)(implicit context: Context): String = {
    s"""
       |verify("${typeVerification.message}", () => {
       |  return ${generateExpression(typeVerification.function.body)};
       |})
     """.stripMargin
  }

  private def generateGenericTypes(genericTypes: Seq[TypeReference]): String = {
    def generateGenericType(genericType: TypeReference): String = {
      genericType.typeName + generateGenericTypes(genericType.genericTypes)
    }
    if (genericTypes.nonEmpty) {
      genericTypes.map(generateGenericType).mkString("<", ",", ">")
    } else {
      ""
    }
  }
}
