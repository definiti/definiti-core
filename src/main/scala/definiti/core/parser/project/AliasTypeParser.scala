package definiti.core.parser.project

import definiti.common.ast._

trait AliasTypeParser {
  self: DefinitiFileParser =>

  import AliasTypeParser._

  def aliasType: Parser[AliasType] = {
    (
      docComment.? ~
        (`transparent` | `opaque`).? ~ `type` ~ identifier ~ container(`[`, joinedElements(identifier, `,`), `]`).? ~
        parameterListDefinition.? ~
        `=` ~ typeDeclaration ~ verificationReference.* ~ aliasTypeBody.?
      ) ^^ {
      case docComment ~ kindOpt ~ firstToken ~ typeName ~ generics ~
        parameters ~
        _ ~ typeDeclaration ~ inherited ~ body =>

        val genericTypes = generics.getOrElse(Seq.empty).map(_.value)
        AliasType(
          kind = kindOpt match {
            case Some(_: TRANSPARENT) => AliasTypeKind.Transparent
            case Some(_: OPAQUE) => AliasTypeKind.Opaque
            case _ => AliasTypeKind.Closed
          },
          name = typeName.value,
          fullName = typeName.value,
          genericTypes = genericTypes,
          parameters = parameters.map(_.value).getOrElse(Seq.empty),
          alias = typeDeclaration,
          inherited = inherited,
          verifications = {
            body
              .map(_.value.verification)
              .getOrElse(Seq.empty)
              .map(injectTypeIntoTypeVerification(_, typeDeclaration.typeName, typeDeclaration.genericTypes.map(_.typeName)))
          },
          methods = {
            body
              .map(_.value.methods)
              .getOrElse(Seq.empty)
          },
          comment = docComment.map(_.value),
          location = location(Range(
            start = position(docComment.orElse(kindOpt).getOrElse(firstToken).pos),
            end = body.map(_.range.end)
              .orElse(inherited.lastOption.map(_.location.range.end))
              .getOrElse(typeDeclaration.location.range.end)
          ))
        )
    }
  }

  def aliasTypeBody: Parser[Ranged[AliasTypeBody]] = {
    rangedContainer(`{`, aliasTypeBodyElement.*, `}`)
      .map { ranged =>
        Ranged(
          value = AliasTypeBody(ranged.value),
          range = ranged.range
        )
      }
  }

  def aliasTypeBodyElement: Parser[AliasTypeBodyElement] = {
    (
      typeVerification.map(AliasTypeVerification)
        | method.map(AliasTypeMethod)
      )
  }

  def method: Parser[Method] = {
    (
      docComment.? ~
        `def` ~ identifier ~ container(`[`, joinedElements(identifier, `,`), `]`).? ~
        parameterListDefinition ~ `:` ~
        typeReference ~ functionBody
      ) ^^ {
      case docComment ~
        firstToken ~ functionName ~ generics ~
        parameters ~ _ ~
        typeReference ~ body =>
        Method(
          name = functionName.value,
          genericTypes = generics.getOrElse(Seq.empty).map(_.value),
          parameters = parameters.value,
          body = body.value,
          returnType = typeReference.value,
          comment = docComment.map(_.value),
          location = location(range(docComment.getOrElse(firstToken), body))
        )
    }
  }

}

object AliasTypeParser {

  case class AliasTypeBody(elements: Seq[AliasTypeBodyElement]) {
    def verification: Seq[TypeVerification] = elements.collect {
      case AliasTypeVerification(typeVerification) => typeVerification
    }

    def methods: Seq[Method] = elements.collect {
      case AliasTypeMethod(method) => method
    }
  }

  sealed trait AliasTypeBodyElement

  case class AliasTypeVerification(typeVerification: TypeVerification) extends AliasTypeBodyElement

  case class AliasTypeMethod(method: Method) extends AliasTypeBodyElement

}
