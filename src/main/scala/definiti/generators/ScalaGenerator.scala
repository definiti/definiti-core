package definiti.generators

import definiti._
import definiti.api.Context

import scala.io.Source

object ScalaGenerator {
  val nativeTypeMapping = Map(
    "Boolean" -> "BooleanWrapper",
    "Date" -> "DateWrapper",
    "List" -> "ListWrapper",
    "Number" -> "NumberWrapper",
    "String" -> "StringWrapper"
  )

  def generate(root: Root)(implicit context: Context): String = {
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

  private def appendNative(buffer: StringBuilder)(implicit context: Context): Unit = {
    Seq("BooleanWrapper", "DateWrapper", "ListWrapper", "NumberWrapper", "StringWrapper") foreach { className =>
      buffer.append(Source.fromResource(s"generators/scala/native/$className.scala").getLines.mkString("", "\n", "\n"))
    }
  }

  private def generateVerification(verification: Verification)(implicit context: Context): String = {
    s"""
       |${verification.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |def verify${verification.name}(${generateParameters(verification.function.parameters)}): Option[String] = {
       |  verify("${verification.message}") {
       |    ${generateExpression(verification.function.body)}
       |  }
       |}
      """.stripMargin
  }

  private def generateParameters(parameterDefinitions: Seq[ParameterDefinition])(implicit context: Context): String = parameterDefinitions match {
    case Nil => ""
    case one :: Nil => generateParameter(one)
    case seq => seq.map(generateParameter).mkString("(", "), (", ")")
  }

  private def generateParametersWithoutExternalBraces(parameterDefinitions: Seq[ParameterDefinition])(implicit context: Context): String = parameterDefinitions match {
    case Nil => ""
    case seq => seq.map(generateParameter).mkString(",")
  }

  private def generateParameter(parameterDefinition: ParameterDefinition)(implicit context: Context): String = {
    val parameterName = parameterDefinition.name
    val parameterType = generateParameterType(parameterDefinition.typeReference)
    s"$parameterName: $parameterType"
  }

  private def generateParameterType(typeReference: AbstractTypeReference)(implicit context: Context): String = {
    typeReference match {
      case TypeReference(typeName, genericTypes) =>
        val finalTypeName = context.findType(typeName) match {
          case Some(_: Type) => "$" + typeName
          case _ => nativeTypeMapping.getOrElse(typeName, typeName)
        }
        val parameterGenerics = generateGenericTypes(genericTypes)
        finalTypeName + parameterGenerics
      case LambdaReference(inputTypes, outputType) =>
        def generateOneType(typeReference: TypeReference): String = {
          val typeName = typeReference.typeName
          val generics = generateGenericTypes(typeReference.genericTypes)
          typeName + generics
        }
        s"(${inputTypes.map(generateOneType)}) => ${generateOneType(outputType)}"
    }
  }

  private def generateExpression(expression: Expression)(implicit context: Context): String = expression match {
    case BooleanValue(value, _) => s"new BooleanWrapper(${value.toString})"
    case NumberValue(value, _) => s"new NumberWrapper(${value.toString})"
    case QuotedStringValue(value, _) => """new StringWrapper("""" + value.toString.replaceAllLiterally("\\", "\\\\") + """")"""
    case Variable(variable, _, _) => variable
    case MethodCall(inner, method, parameters, _, _) =>
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
    case LambdaExpression(parameterList, inner, _) => s"(${generateParametersWithoutExternalBraces(parameterList)}) => {${generateExpression(inner)}}"
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
      .map(parameter => s"val $parameter = __result")
      .mkString("\n")

    val verifications = (
      definedType.verifications.map(generateTypeVerification)
        ++
        definedType.inherited.map(inherited => s"verify$inherited(__result)")
      ).mkString(", ")

    val realType = originalTypeOpt.map(_.typeName).getOrElse(definedType.name)

    val resultType = originalTypeOpt match {
      case Some(_) =>
        s"new ${definedType.name}(${definedType.attributes.map(attribute => s"__result.${attribute.name}").mkString(", ")})"
      case None =>
        "__result"
    }

    val $interface = originalTypeOpt.map(_ => "").getOrElse(generateInterface(definedType))

    val originalTypeGenerics = originalTypeOpt match {
      case Some(originalType) => generateGenericTypes(originalType.genericTypes)
      case None => generateGenericTypeDefinition(definedType)
    }

    val typeDefinition = s"${generateGenericTypeDefinition(definedType)}"
    s"""
       |${$interface}
       |${definedType.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |class ${definedType.name}$typeDefinition private(${generateAttributes(definedType.attributes)}) extends $$$realType$originalTypeGenerics
       |
       |object ${definedType.name} {
       |  def apply$typeDefinition(${generateAttributeParameters(definedType.attributes)}): Either[String, ${definedType.name}$typeDefinition] = {
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

  private def generateInterface(definedType: DefinedType)(implicit context: Context): String = {
    s"""
       |trait $$${definedType.name}${generateGenericTypeDefinition(definedType)} {
       |  ${definedType.attributes.map(attribute => "def " + generateAttributeParameter(attribute)).mkString("\n")}
       |}
     """.stripMargin
  }

  private def generateGenericTypeDefinition(definedType: DefinedType) = {
    if (definedType.genericTypes.nonEmpty) {
      definedType.genericTypes.mkString("[", ",", "]")
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
    s"val ${generateAttributeParameter(attributeDefinition)}"
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
       |verify("${typeVerification.message}") {
       |  ${generateExpression(typeVerification.function.body)}
       |}
     """.stripMargin
  }

  private def generateGenericTypes(genericTypes: Seq[TypeReference]): String = {
    def generateGenericType(genericType: TypeReference): String = {
      nativeTypeMapping.getOrElse(genericType.typeName, genericType.typeName) + generateGenericTypes(genericType.genericTypes)
    }
    if (genericTypes.nonEmpty) {
      genericTypes.map(generateGenericType).mkString("[", ",", "]")
    } else {
      ""
    }
  }
}
