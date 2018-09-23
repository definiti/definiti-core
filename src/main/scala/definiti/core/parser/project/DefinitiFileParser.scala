package definiti.core.parser.project

import definiti.common.ast._
import definiti.common.utils.StringUtils
import definiti.common.validation._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.Reader

class DefinitiFileParser(filename: String)
  extends PackratParsers
    with ProjectParserHelper
    with ExpressionParser {

  type Elem = TokenProject

  def parse(reader: Reader[TokenProject]): Validated[FileContent] = {
    contentFile(reader) match {
      case Success(result, _) =>
        Valid(result)

      case Failure(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, next.pos.line, next.pos.column, next.pos.line, next.pos.column)
        )

      case Error(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, next.pos.line, next.pos.column, next.pos.line, next.pos.column)
        )
    }
  }

  def contentFile: Parser[FileContent] = {
    (packageName.? ~ importLine.* ~ namespaceElements) ^^ {
      case packageName ~ importLines ~ namespaceElements =>
        FileContent(
          filename = filename,
          packageName = packageName.getOrElse(""),
          imports = importLines.map { importLine =>
            StringUtils.lastPart(importLine) -> importLine
          }.toMap,
          elements = namespaceElements
        )
    }
  }

  def packageName: Parser[String] = {
    (PACKAGE() ~ dottedIdentifier) ^^ {
      case _ ~ packageName => packageName
    }
  }

  def importLine: Parser[String] = {
    (IMPORT() ~ dottedIdentifier) ^^ {
      case _ ~ importLine => importLine
    }
  }

  def namespaceElements: Parser[Seq[NamespaceElement]] = {
    phrase(rep1(
      verificationParser
        | aliasType
        | definedType
        | enumType
        | namedFunction
        | extendedContext
    ))
  }

  def verificationParser: Parser[Verification] = {
    (
      docComment.? ~ `verification` ~ identifier ~
        parameterListDefinition.? ~
        `{` ~ verificationMessage ~ function ~ `}`
      ) ^^ {
      case docComment ~ verification ~ verificationName ~
        parameterDefinitions ~
        _ ~ verificationMessage ~ function ~ rightBrace =>
        Verification(
          name = verificationName.value,
          fullName = verificationName.value,
          parameters = parameterDefinitions.map(_.value).getOrElse(Seq.empty),
          message = verificationMessage,
          function = function,
          comment = docComment.map(_.value),
          location = location(range(docComment.getOrElse(verification), rightBrace))
        )
    }
  }

  def parameterListDefinition: Parser[Ranged[Seq[ParameterDefinition]]] = {
    rangedContainer(`(`, joinedElements(parameterDefinition, `,`).?, `)`)
      .map { ranged =>
        ranged.copy(
          value = ranged.value.getOrElse(Seq.empty)
        )
      }
  }

  def parameterDefinition: Parser[ParameterDefinition] = {
    (identifier ~ (`:` ~ typeReference).?) ^^ {
      case parameterName ~ typeReferenceDeclaration =>
        val typeReference = typeReferenceDeclaration
          .map { case _ ~ declaredTypeReference => declaredTypeReference.value }
          .getOrElse {
            TypeReference(typeName = parameterName.value.capitalize)
          }
        ParameterDefinition(
          name = parameterName.value,
          typeReference = typeReference,
          location = location(Range(
            position(parameterName.pos),
            typeReferenceDeclaration
              .map(_._2.range.end)
              .getOrElse(position(parameterName.posEnd))
          ))
        )
    }
  }

  def typeReference: Parser[Ranged[TypeReference]] = {
    (rangedDottedIdentifier ~ rangedContainer(`[`, genericTypeList, `]`).?) ^^ {
      case name ~ genericsOpt =>
        val generics = genericsOpt.map(_.value).getOrElse(Seq.empty)
        Ranged(
          value = TypeReference(
            typeName = name.value,
            genericTypes = generics.map(_.value)
          ),
          range = range(name, genericsOpt.getOrElse(name))
        )
    }
  }

  def typeDeclaration: Parser[TypeDeclaration] = {
    (
      rangedDottedIdentifier ~ rangedContainer(`[`, typeDeclarationList, `]`).? ~
        rangedContainer(`(`, joinedElements(atomicExpression, `,`), `)`).?
      ) ^^ {
      case name ~ generics ~ parameters =>
        TypeDeclaration(
          typeName = name.value,
          genericTypes = generics.map(_.value).getOrElse(Seq.empty),
          parameters = parameters.map(_.value).getOrElse(Seq.empty),
          location = location(range(name, parameters.orElse(generics).getOrElse(name)))
        )
    }
  }

  def typeDeclarationList: Parser[Seq[TypeDeclaration]] = {
    joinedElements(typeDeclaration, `,`)
  }

  def genericTypeList: Parser[Seq[Ranged[TypeReference]]] = {
    joinedElements(typeReference, `,`)
  }

  def verificationMessage: Parser[VerificationMessage] = {
    literalMessage | typedMessage
  }

  def literalMessage: Parser[LiteralMessage] = {
    string.map { message =>
      LiteralMessage(
        message = message.value,
        location = location(range(message, message))
      )
    }
  }

  def typedMessage: Parser[TypedMessage] = {
    (`message` ~ `(` ~ string ~ (`,` ~ typeReference).* ~ `)`) ^^ {
      case firstToken ~ _ ~ message ~ typeReferencesWithComma ~ lastToken =>
        TypedMessage(
          message = message.value,
          types = typeReferencesWithComma.map { case _ ~ typeReference => typeReference.value },
          location = location(range(firstToken, lastToken))
        )
    }
  }

  def function: Parser[DefinedFunction] = {
    (
      rangedContainer(`[`, joinedElements(identifier, `,`), `]`).? ~
        parameterListDefinition ~
        `{` ~ combinedExpression ~ `}`
      ) ^^ {
      case generics ~ parameters ~ _ ~ expression ~ lastToken =>
        DefinedFunction(
          parameters = parameters.value,
          body = expression,
          genericTypes = generics.map(_.value.map(_.value)).getOrElse(Seq.empty),
          location = location(range(generics.getOrElse(parameters), lastToken))
        )
    }
  }

  def definedType: Parser[DefinedType] = {
    (
      docComment.? ~
        `type` ~ identifier ~ container(`[`, joinedElements(identifier, `,`), `]`).? ~
        parameterListDefinition.? ~ verificationReference.* ~
        `{` ~ attributeDefinition.* ~ typeVerification.* ~ `}`
      ) ^^ {
      case docComment ~
        typeToken ~ typeName ~ generics ~
        parameters ~ verifyingList ~
        _ ~ attributes ~ typeVerifications ~ lastToken =>

        val genericTypes = generics.getOrElse(Seq.empty).map(_.value)
        DefinedType(
          name = typeName.value,
          fullName = typeName.value,
          genericTypes = genericTypes,
          parameters = parameters.map(_.value).getOrElse(Seq.empty),
          attributes = attributes,
          verifications = typeVerifications.map(injectTypeIntoTypeVerification(_, typeName.value, genericTypes)),
          inherited = verifyingList,
          comment = docComment.map(_.value),
          location = location(range(docComment.getOrElse(typeToken), lastToken))
        )
    }
  }

  def injectTypeIntoTypeVerification(typeVerification: TypeVerification, typeName: String, generics: Seq[String]): TypeVerification = {
    typeVerification match {
      case atomicTypeVerification: AtomicTypeVerification =>
        atomicTypeVerification.copy(
          function = atomicTypeVerification.function.copy(
            parameters = atomicTypeVerification.function.parameters.map { parameter =>
              if (parameter.typeReference == unset) {
                parameter.copy(typeReference = TypeReference(typeName, generics.map(TypeReference(_, Seq.empty))))
              } else {
                parameter
              }
            }
          )
        )
      case dependentTypeVerification: DependentTypeVerification =>
        dependentTypeVerification.copy(
          function = dependentTypeVerification.function.copy(
            parameters = dependentTypeVerification.function.parameters.map { parameter =>
              if (parameter.typeReference == unset) {
                parameter.copy(typeReference = TypeReference(typeName, generics.map(TypeReference(_, Seq.empty))))
              } else {
                parameter
              }
            }
          )
        )
    }
  }

  def verificationReference: Parser[VerificationReference] = {
    (
      `verifying` ~ identifier ~
        rangedContainer(`(`, joinedElements(atomicExpression, `,`).?, `)`).?
      ) ^^ {
      case firstToken ~ verificationName ~ parameters =>
        VerificationReference(
          verificationName = verificationName.value,
          parameters = parameters.flatMap(_.value).getOrElse(Seq.empty),
          location = location(
            Range(
              position(firstToken.pos),
              parameters.map(_.range.end).getOrElse(position(verificationName.posEnd))
            )
          )
        )
    }
  }

  def attributeDefinition: Parser[AttributeDefinition] = {
    (
      docComment.? ~ identifier ~ (`:` ~ typeDeclaration).? ~
        verificationReference.* ~ (`as` ~ `transparent`.? ~ identifier).?
      ) ^^ {
      case docComment ~ attributeName ~ typeDeclarationOpt ~
        verifyingList ~ asOpt =>
        AttributeDefinition(
          name = attributeName.value,
          typeDeclaration = {
            typeDeclarationOpt.map {
              case _ ~ typeDeclaration => typeDeclaration
            }.getOrElse {
              TypeDeclaration(attributeName.value.capitalize, Seq.empty, Seq.empty, location(range(attributeName, attributeName)))
            }
          },
          comment = docComment.map(_.value),
          verifications = verifyingList,
          attributeType = asOpt.map { case _ ~ transparentOpt ~ typeName =>
            AttributeType(
              kind = transparentOpt.map(_ => AliasTypeKind.Transparent).getOrElse(AliasTypeKind.Closed),
              name = typeName.value
            )
          },
          location = location(Range(
            start = position(docComment.getOrElse(attributeName).pos),
            end = asOpt
              .map { case _ ~ _ ~ token => position(token.posEnd) }
              .orElse {
                verifyingList.lastOption.map(_.location.range.end)
              }
              .orElse(typeDeclarationOpt.map { case _ ~ typeDeclaration => typeDeclaration.location.range.end })
              .getOrElse(position(attributeName.posEnd))
          ))
        )
    }
  }

  def typeVerification: Parser[TypeVerification] = {
    atomicTypeVerification | dependentTypeVerification
  }

  def atomicTypeVerification: Parser[AtomicTypeVerification] = {
    (
      `verify` ~ `{` ~
        verificationMessage ~ typeVerificationFunction ~
        `}`
      ) ^^ {
      case firstToken ~ _ ~ verificationMessage ~ definedFunction ~ lastToken =>
        AtomicTypeVerification(
          message = verificationMessage,
          function = definedFunction,
          location = location(range(firstToken, lastToken))
        )
    }
  }

  def dependentTypeVerification: Parser[DependentTypeVerification] = {
    (
      `verify` ~ identifier ~ parameterListDefinition ~ `{` ~
        verificationMessage ~ typeVerificationFunction ~
        `}`
      ) ^^ {
      case firstToken ~ verificationName ~ parameters ~ _ ~
        verificationMessage ~ definedFunction ~
        lastToken =>

        DependentTypeVerification(
          name = verificationName.value,
          message = verificationMessage,
          function = definedFunction.copy(
            parameters = definedFunction.parameters ++ parameters.value
          ),
          location = location(range(firstToken, lastToken))
        )
    }
  }

  def typeVerificationFunction: Parser[DefinedFunction] = {
    (
      rangedContainer(`(`, identifier, `)`) ~ rangedContainer(`{`, combinedExpression, `}`)
      ) ^^ {
      case attributeName ~ expression =>
        DefinedFunction(
          parameters = Seq(ParameterDefinition(
            name = attributeName.value.value,
            typeReference = unset,
            location = location(range(attributeName.value, attributeName.value))
          )),
          body = expression.value,
          genericTypes = Seq.empty,
          location = location(range(attributeName, expression))
        )
    }
  }

  def aliasType: Parser[AliasType] = {
    (
      docComment.? ~
        `transparent`.? ~ `type` ~ identifier ~ container(`[`, joinedElements(identifier, `,`), `]`).? ~
        parameterListDefinition.? ~
        `=` ~ typeDeclaration ~ verificationReference.* ~ aliasTypeBody.?
      ) ^^ {
      case docComment ~ transparentOpt ~ firstToken ~ typeName ~ generics ~
        parameters ~
        _ ~ typeDeclaration ~ inherited ~ body =>

        val genericTypes = generics.getOrElse(Seq.empty).map(_.value)
        AliasType(
          kind = transparentOpt.map(_ => AliasTypeKind.Transparent).getOrElse(AliasTypeKind.Closed),
          name = typeName.value,
          fullName = typeName.value,
          genericTypes = genericTypes,
          parameters = parameters.map(_.value).getOrElse(Seq.empty),
          alias = typeDeclaration,
          inherited = inherited,
          verifications = {
            body
              .map(_.value)
              .getOrElse(Seq.empty)
              .map(injectTypeIntoTypeVerification(_, typeDeclaration.typeName, typeDeclaration.genericTypes.map(_.typeName)))
          },
          comment = docComment.map(_.value),
          location = location(Range(
            start = position(docComment.orElse(transparentOpt).getOrElse(firstToken).pos),
            end = body.map(_.range.end)
              .orElse(inherited.lastOption.map(_.location.range.end))
              .getOrElse(typeDeclaration.location.range.end)
          ))
        )
    }
  }

  def aliasTypeBody: Parser[Ranged[Seq[TypeVerification]]] = {
    rangedContainer(`{`, typeVerification.*, `}`)
      .map { ranged =>
        ranged.copy(
          value = Seq(ranged.value: _*)
        )
      }
  }

  def enumType: Parser[Enum] = {
    (
      docComment.? ~ `enum` ~ identifier ~ rangedContainer(`{`, enumCase.*, `}`)
      ) ^^ {
      case docComment ~ firstToken ~ enumName ~ enumCases =>
        Enum(
          name = enumName.value,
          fullName = enumName.value,
          cases = Seq(enumCases.value: _*),
          comment = docComment.map(_.value),
          location = location(range(
            docComment.getOrElse(firstToken),
            enumCases
          ))
        )
    }
  }

  def enumCase: Parser[EnumCase] = {
    (
      docComment.? ~ identifier
      ) ^^ {
      case docComment ~ caseName =>
        EnumCase(
          name = caseName.value,
          comment = docComment.map(_.value),
          location = location(range(docComment.getOrElse(caseName), caseName))
        )
    }
  }

  def namedFunction: Parser[NamedFunction] = {
    (
      `def` ~ identifier ~ container(`[`, joinedElements(identifier, `,`), `]`).? ~
        parameterListDefinition ~ `:` ~
        typeReference ~ namedFunctionBody
      ) ^^ {
      case firstToken ~ functionName ~ generics ~
        parameters ~ _ ~
        typeReference ~ body =>
        NamedFunction(
          name = functionName.value,
          fullName = functionName.value,
          genericTypes = generics.getOrElse(Seq.empty).map(_.value),
          parameters = parameters.value,
          returnType = typeReference.value,
          body = body.value,
          location = location(range(firstToken, body))
        )
    }
  }

  def namedFunctionBody: Parser[Ranged[Expression]] = {
    val chained: Parser[Ranged[Expression]] = {
      rangedContainer(`{`, combinedExpression, `}`)
        .map { chaine =>
          Ranged(
            value = chaine.value,
            range = chaine.range
          )
        }
    }
    val simple: Parser[Ranged[Expression]] = expression.map(expression => Ranged(expression, expression.location.range))

    chained | simple
  }

  def extendedContext: Parser[ExtendedContext[_]] = {
    (
      `context` ~ identifier ~ contextContent
      ) ^^ {
      case firstToken ~ name ~ content =>
        ExtendedContext(
          name = name.value,
          content = content.content,
          innerLocation = location(Range(
            {
              val start = position(content.pos)
              start.copy(column = start.column + 3)
            },
            {
              val end = position(content.posEnd)
              end.copy(column = end.column - 3)
            }
          )),
          location = location(range(firstToken, content))
        )
    }
  }

  def combinedExpression: Parser[Expression] = {
    expression.+ ^^ {
      expressions =>
        expressions match {
          case head :: Nil =>
            head

          case parts => CombinedExpression(
            parts = parts,
            returnType = expressions.last.returnType,
            location = location(Range(
              start = expressions.head.location.range.start,
              end = expressions.last.location.range.end
            ))
          )
        }
    }
  }

  def location(range: Range): Location = {
    Location(filename, range)
  }

  def location(token: TokenProject): Location = {
    Location(filename, range(token, token))
  }
}
