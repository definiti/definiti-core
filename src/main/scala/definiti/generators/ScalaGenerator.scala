package definiti.generators

import definiti._
import definiti.api.TypeReference

import scala.io.Source

object ScalaGenerator {
  val nativeTypeMapping = Map(
    "Boolean" -> "BooleanWrapper",
    "Date" -> "DateWrapper",
    "Number" -> "NumberWrapper",
    "String" -> "StringWrapper"
  )

  def generate(root: Root): String = {
    val buffer: StringBuilder = new StringBuilder()

    appendNative(buffer)

    buffer.append(
      s"""
         |package object verifications {
         |  ${root.verifications.map(generateVerification).mkString("\n\n")}
         |
         |  private def verify(message: String)(condition: => Boolean) = {
         |    if (condition) {
         |      None
         |    } else {
         |      Some(message)
         |    }
         |  }
         |}
         |import verifications._
      """.stripMargin
    )

    buffer.append(
      s"""
         |${root.classDefinitions.map(generateClassDefinition).mkString("\n\n")}
       """.stripMargin
    )

    buffer.toString()
  }

  private def appendNative(buffer: StringBuilder): Unit = {
    Seq("BooleanWrapper", "DateWrapper", "NumberWrapper", "StringWrapper") foreach { className =>
      buffer.append(Source.fromResource(s"generators/scala/native/$className.scala").getLines.mkString("", "\n", "\n"))
    }
  }

  private def generateVerification(verification: Verification): String = {
    s"""
       |${verification.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |def verify${verification.name}(${generateParameters(verification.function.parameters)}): Option[String] = {
       |  verify("${verification.message}") {
       |    ${generateExpression(verification.function.body)}
       |  }
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
    case BooleanValue(value, _) => s"new BooleanWrapper(${value.toString})"
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
          s"""
             |if (${generateExpression(condition)}) {
             |  ${generateExpression(onTrue)}
             |} else {
             |  ${generateExpression(onFalseBody)}
             |}
             |""".stripMargin
        case None =>
          s"""
             |if (${generateExpression(condition)}) {
             |  ${generateExpression(onTrue)}
             |}
             |""".stripMargin
      }
    case Or(left, right, _) => s"(${generateExpression(left)}) || (${generateExpression(right)})"
    case And(left, right, _) => s"(${generateExpression(left)}) && (${generateExpression(right)})"
    case Equal(left, right, _) => s"(${generateExpression(left)}) == (${generateExpression(right)})"
    case NotEqual(left, right, _) => s"(${generateExpression(left)}) != (${generateExpression(right)})"
    case Lower(left, right, _) => s"(${generateExpression(left)}) < (${generateExpression(right)})"
    case Upper(left, right, _) => s"(${generateExpression(left)}) > (${generateExpression(right)})"
    case LowerOrEqual(left, right, _) => s"(${generateExpression(left)}) <= (${generateExpression(right)})"
    case UpperOrEqual(left, right, _) => s"(${generateExpression(left)}) >= (${generateExpression(right)})"
    case Plus(left, right, _) => s"(${generateExpression(left)}) + (${generateExpression(right)})"
    case Minus(left, right, _) => s"(${generateExpression(left)}) - (${generateExpression(right)})"
    case Modulo(left, right, _) => s"(${generateExpression(left)}) % (${generateExpression(right)})"
    case Time(left, right, _) => s"(${generateExpression(left)}) * (${generateExpression(right)})"
    case Divide(left, right, _) => s"(${generateExpression(left)}) / (${generateExpression(right)})"
    case Not(inner, _) => s"!(${generateExpression(inner)})"
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
      .map(parameter => s"val $parameter = __result")
      .mkString("\n")

    val verifications = (
      definedType.verifications.map(generateTypeVerification)
        ++
        definedType.inherited.map(inherited => s"verify$inherited(__result)")
      ).mkString(", ")

    val originalType = originalTypeOpt.getOrElse(definedType.name)

    val resultType = originalTypeOpt match {
      case Some(_) =>
        s"new ${definedType.name}(${definedType.attributes.map(attribute => s"__result.${attribute.name}").mkString(", ")})"
      case None =>
        "__result"
    }

    val $interface = originalTypeOpt.map(_ => "").getOrElse(generateInterface(definedType))

    s"""
       |${$interface}
       |${definedType.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |class ${definedType.name} private(${generateAttributes(definedType.attributes)}) extends $$${originalType}
       |
       |object ${definedType.name} {
       |  def apply(${generateAttributeParameters(definedType.attributes)}): Either[String, ${definedType.name}] = {
       |    val __result = new ${definedType.name}(${definedType.attributes.map(_.name).mkString(", ")})
       |    ${__resultAliases}
       |    val __errorOpt = Seq(
       |      $verifications
       |    ).find(_.isDefined).flatten
       |
       |    __errorOpt match {
       |      case Some(error) => Left(error)
       |      case None =>
       |
       |      Right($resultType)
       |    }
       |  }
       |
       |  private def verify(message: String)(condition: => Boolean) = {
       |    if (condition) {
       |      None
       |    } else {
       |      Some(message)
       |    }
       |  }
       |}
     """.stripMargin
  }

  private def generateInterface(definedType: DefinedType): String = {
    s"""
       |trait $$${definedType.name} {
       |  ${definedType.attributes.map(attribute => "def " + generateAttributeParameter(attribute)).mkString("\n")}
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
       |verify("${typeVerification.message}") {
       |  ${generateExpression(typeVerification.function.body)}
       |}
     """.stripMargin
  }
}
