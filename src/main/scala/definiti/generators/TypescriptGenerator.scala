package definiti.generators

import definiti._
import definiti.api.TypeReference

import scala.io.Source

object TypescriptGenerator {
  val nativeTypeMapping = Map(
    "Date" -> "DateWrapper",
    "Number" -> "NumberWrapper",
    "String" -> "StringWrapper"
  )

  def generate(root: Root): String = {
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

  private def appendNative(buffer: StringBuilder): Unit = {
    Seq("DateWrapper", "NumberWrapper", "StringWrapper") foreach { className =>
      buffer.append(Source.fromResource(s"generators/ts/native/$className.ts").getLines.mkString("", "\n", "\n"))
    }
  }

  private def generateVerification(verification: Verification): String = {
    s"""
       |${verification.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |export function verify${verification.name}(${generateParameters(verification.function.parameters)}): string|null {
       |  return verify("${verification.message}", () => {
       |    return ${generateExpression(verification.function.body)};
       |  })
       |}
      """.stripMargin
  }

  private def generateParameters(parameterDefinitions: Seq[ParameterDefinition]): String = parameterDefinitions match {
    case Nil => ""
    case one :: Nil => generateParameter(one)
    case seq => seq.map(generateParameter).mkString("(", "), (", ")")
  }

  private def generateParameter(parameterDefinition: ParameterDefinition): String = {
    val parameterType = TypeReference.findType(parameterDefinition.typeReference) match {
      case Some(_: Type) => "$" + parameterDefinition.typeReference
      case _ => parameterDefinition.typeReference
    }
    s"${parameterDefinition.name}: ${nativeTypeMapping.getOrElse(parameterType, parameterType)}"
  }

  private def generateExpression(expression: Expression): String = expression match {
    case BooleanValue(value) => s"${value.toString}"
    case NumberValue(value) => s"new NumberWrapper(${value.toString})"
    case QuotedStringValue(value) => """new StringWrapper("""" + value.toString.replaceAllLiterally("\\", "\\\\") + """")"""
    case Variable(variable, _) => variable
    case MethodCall(inner, method, parameters) =>
      s"(${generateExpression(inner)}).$method(${generateCallParameters(parameters)})"
    case AttributeCall(inner, attribute) =>
      s"(${generateExpression(inner)}).$attribute"
    case CombinedExpression(expressions) =>
      expressions.map(generateExpression).mkString("\n")
    case Condition(condition, onTrue, onFalse) =>
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
    case Or(left, right) => logicalExpression("||", "or", left, right)
    case And(left, right) => logicalExpression("&&", "and", left, right)
    case Equal(left, right) => logicalExpression("==", "equals", left, right)
    case NotEqual(left, right) => logicalExpression("!=", "notEquals", left, right)
    case Lower(left, right) => logicalExpression("<", "lower", left, right)
    case Upper(left, right) => logicalExpression(">", "upper", left, right)
    case LowerOrEqual(left, right) => logicalExpression("<=", "lowerOrEquals", left, right)
    case UpperOrEqual(left, right) => logicalExpression(">=", "upperOrEquals", left, right)
    case Plus(left, right) => logicalExpression("+", "plus", left, right)
    case Minus(left, right) => logicalExpression("-", "minus", left, right)
    case Modulo(left, right) => logicalExpression("%", "modulo", left, right)
    case Time(left, right) => logicalExpression("*", "time", left, right)
    case Divide(left, right) => logicalExpression("/", "divide", left, right)
    case Not(inner) => s"!(${generateExpression(inner)})"
  }

  private def logicalExpression(symbol: String, wrapperMethod: String, left: Expression, right: Expression): String = {
    if (nativeTypeMapping.contains(left.returnType.name)) {
      s"(${generateExpression(left)}).$wrapperMethod(${generateExpression(right)})"
    } else {
      s"(${generateExpression(left)}) $symbol (${generateExpression(right)})"
    }
  }

  private def generateCallParameters(expressions: Seq[Expression]): String = expressions match {
    case Nil => ""
    case one :: Nil => generateExpression(one)
    case seq => seq.map(generateExpression).mkString("(", "), (", ")")
  }

  private def generateClassDefinition(classDefinition: ClassDefinition): String = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
  }

  private def generateDefinedType(definedType: DefinedType, originalTypeOpt: Option[String] = None): String = {
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

    val originalType = originalTypeOpt.getOrElse(definedType.name)

    val $interface = originalTypeOpt.map(_ => "").getOrElse(generateInterface(definedType))

    s"""
       |${$interface}
       |${definedType.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |export interface ${definedType.name} extends $$$originalType {}
       |
       |export function ${definedType.name}(${generateAttributeParameters(definedType.attributes)}): string|Readonly<${definedType.name}> {
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

  private def generateInterface(definedType: DefinedType): String = {
    s"""
       |interface $$${definedType.name} {
       |  ${definedType.attributes.map(attribute => generateAttributeParameter(attribute) + ";").mkString("\n")}
       |}
     """.stripMargin
  }

  private def generateAliasType(aliasType: AliasType): String = {
    TypeReference.findType(aliasType.alias) match {
      case Some(definedType: DefinedType) =>
        generateDefinedType(definedType.copy(
          comment = aliasType.comment,
          name = aliasType.name,
          inherited = definedType.inherited ++ aliasType.inherited
        ), Some(aliasType.alias))
      case _ => throw new RuntimeException("Undefined type: " + aliasType)
    }
  }

  private def generateAttributes(attributeDefinition: Seq[AttributeDefinition]): String = {
    attributeDefinition.map(generateAttribute).mkString(", ")
  }

  private def generateAttribute(attributeDefinition: AttributeDefinition): String = {
    s"val ${attributeDefinition.name}: ${nativeTypeMapping.getOrElse(attributeDefinition.typeReference, attributeDefinition.typeReference)}"
  }

  private def generateAttributeParameters(attributeDefinition: Seq[AttributeDefinition]): String = {
    attributeDefinition.map(generateAttributeParameter).mkString(", ")
  }

  private def generateAttributeParameter(attributeDefinition: AttributeDefinition): String = {
    s"${attributeDefinition.name}: ${nativeTypeMapping.getOrElse(attributeDefinition.typeReference, attributeDefinition.typeReference)}"
  }

  private def generateTypeVerification(typeVerification: TypeVerification): String = {
    s"""
       |verify("${typeVerification.message}", () => {
       |  return ${generateExpression(typeVerification.function.body)};
       |})
     """.stripMargin
  }
}
