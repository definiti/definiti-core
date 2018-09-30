package definiti.core.parser.api

import definiti.common.ast.{Position => ASTPosition, _}
import definiti.common.validation._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Position

class ParserCore(filename: String) extends Parsers {
  type Elem = TokenCore

  def parse(value: Seq[TokenCore]): Validated[Seq[ClassDefinition]] = {
    classDefinition(new TokenCoreReader(value)) match {
      case Success(result, _) =>
        Valid(result)

      case Failure(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, 0, 0, next.pos.line, next.pos.column)
        )

      case Error(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, 0, 0, next.pos.line, next.pos.column)
        )
    }
  }

  private def classDefinition: Parser[Seq[ClassDefinition]] = {
    phrase(rep1(
      (
        docComment.? ~
          TYPE() ~ identifier ~ genericTypeListContainerOpt ~
          LEFT_BRACE() ~ member.* ~ RIGHT_BRACE()
        ) ^^ {
        case docComment ~ _ ~ typeName ~ genericTypeList ~ _ ~ members ~ _ =>
          val attributes = members.collect {
            case Left(attributeDefinition) => attributeDefinition
          }
          val methods = members.collect {
            case Right(methodDefinition) => methodDefinition
          }
          NativeClassDefinition(
            name = typeName.value,
            fullName = typeName.value,
            genericTypes = genericTypeList,
            attributes = attributes,
            methods = methods,
            comment = docComment.map(_.value)
          )
      }
    ))
  }

  private def genericTypeListContainerOpt: Parser[Seq[String]] = {
    (LEFT_BRACKET() ~ genericTypeList ~ RIGHT_BRACKET()).? ^^ {
      case Some(_ ~ genericTypes ~ _) => genericTypes
      case _ => Seq.empty
    }
  }

  private def genericTypeList: Parser[Seq[String]] = {
    (identifier ~ COMMA()).* ~ identifier ^^ {
      case identifierCommaSeq ~ identifier =>
        val previousIdentifiers = identifierCommaSeq.map {
          case innerIdentifier ~ _ => innerIdentifier.value
        }
        val lastIdentifier = identifier.value
        previousIdentifiers :+ lastIdentifier
    }
  }

  private def member: Parser[Either[AttributeDefinition, MethodDefinition]] = {
    attributeDefinition.map(Left(_)) | methodDefinition.map(Right(_))
  }

  private def attributeDefinition: Parser[AttributeDefinition] = {
    (
      docComment.? ~ identifier ~ COLON() ~ typeReference
      ) ^^ {
      case docComment ~ attributeName ~ _ ~ typeDeclaration =>
        AttributeDefinition(
          name = attributeName.value,
          typeDeclaration = typeDeclaration,
          comment = docComment.map(_.value),
          verifications = Seq.empty,
          attributeType = None,
          location = Location(
            file = filename,
            start = docComment.getOrElse(attributeName).pos,
            end = typeDeclaration.location.range.end
          )
        )
    }
  }

  private def methodDefinition: Parser[MethodDefinition] = {
    (
      docComment.? ~ identifier ~ genericTypeListContainerOpt ~
        LEFT_PARENTHESIS() ~ parameterListDefinition ~ RIGHT_PARENTHESIS() ~
        COLON() ~ typeReference
      ) ^^ {
      case docComment ~ methodName ~ genericTypes ~
        _ ~ parameterDefinitions ~ _ ~
        _ ~ methodType =>
        MethodDefinition(
          name = methodName.value,
          genericTypes = genericTypes,
          parameters = parameterDefinitions,
          returnType = toTypeReference(methodType),
          comment = docComment.map(_.value)
        )
    }
  }

  private def parameterListDefinition: Parser[Seq[ParameterDefinition]] = {
    (
      (parameterDefinition ~ COMMA()).* ~ parameterDefinition
      ).? ^^ {
      case Some(parameters) =>
        parameters match {
          case parameterCommaSeq ~ lastParameter =>
            val previousParameters = parameterCommaSeq.map {
              case innerParameter ~ _ => innerParameter
            }
            previousParameters :+ lastParameter
        }
      case None =>
        Seq.empty
    }
  }

  private def parameterDefinition: Parser[ParameterDefinition] = {
    (
      identifier ~ COLON() ~ abstractTypeReference
      ) ^^ {
      case parameterName ~ _ ~ abstractTypeReference =>
        ParameterDefinition(
          name = parameterName.value,
          typeReference = abstractTypeReference.value,
          location = Location(
            file = filename,
            start = parameterName.pos,
            end = abstractTypeReference.range.end
          )
        )
    }
  }

  private def abstractTypeReference: Parser[Located[AbstractTypeReference]] = {
    (lambdaReference.map(Left(_)) | typeReference.map(Right(_))).map {
      case Left(lambdaReference: Located[LambdaReference]) =>
        Located(
          value = lambdaReference.value,
          range = lambdaReference.range
        )
      case Right(typeDeclaration: TypeDeclaration) =>
        Located(
          value = toTypeReference(typeDeclaration),
          range = typeDeclaration.location.range
        )
    }
  }

  private def lambdaReference: Parser[Located[LambdaReference]] = {
    simpleLambdaReference | complexLambdaReference
  }

  private def simpleLambdaReference: Parser[Located[LambdaReference]] = {
    (typeReference ~ RIGHT_ARROW() ~ typeReference) ^^ {
      case input ~ _ ~ output =>
        Located(
          value = LambdaReference(Seq(toTypeReference(input)), toTypeReference(output)),
          range = Range(input.location.range.start, output.location.range.end)
        )
    }
  }

  private def complexLambdaReference: Parser[Located[LambdaReference]] = {
    (LEFT_PARENTHESIS() ~ typeReferenceList ~ RIGHT_PARENTHESIS() ~ RIGHT_ARROW() ~ typeReference) ^^ {
      case head ~ typeReferenceList ~ _ ~ _ ~ output =>
        Located(
          value = LambdaReference(typeReferenceList.map(toTypeReference), toTypeReference(output)),
          range = Range(head.pos, output.location.range.end)
        )
    }
  }

  private def typeReference: Parser[TypeDeclaration] = {
    (
      identifier ~ (LEFT_BRACKET() ~ typeReferenceList ~ RIGHT_BRACKET()).?
      ) ^^ {
      case identifier ~ innerTypeReferenceContainer =>
        TypeDeclaration(
          typeName = identifier.value,
          genericTypes = innerTypeReferenceContainer
            .map { case _ ~ typeReferenceList ~ _ => typeReferenceList }
            .getOrElse(Seq.empty),
          parameters = Seq.empty,
          location = Location(
            file = filename,
            range = range(identifier, innerTypeReferenceContainer.map { case _ ~ _ ~ last => last }.getOrElse(identifier))
          )
        )
    }
  }

  private def typeReferenceList: Parser[Seq[TypeDeclaration]] = {
    (
      (typeReference ~ COMMA()).* ~ typeReference
      ) ^^ {
      case typeReferenceCommaSeq ~ lastTypeReference =>
        val previousTypeReferences = typeReferenceCommaSeq.map {
          case innerTypeReference ~ _ => innerTypeReference
        }
        previousTypeReferences :+ lastTypeReference
    }
  }

  private def docComment: Parser[DOC_COMMENT] = {
    accept("docComment", { case docComment: DOC_COMMENT => docComment })
  }

  private def identifier: Parser[IDENTIFIER] = {
    accept("identifier", { case identifier: IDENTIFIER => identifier })
  }

  implicit private def position(pos: Position): ASTPosition = {
    ASTPosition(
      line = pos.line,
      column = pos.column
    )
  }

  private def range(firstToken: TokenCore, lastToken: TokenCore): Range = {
    Range(
      startLine = firstToken.pos.line,
      startColumn = firstToken.pos.column,
      endLine = lastToken.posEnd.line,
      endColumn = lastToken.posEnd.column
    )
  }

  private def toTypeReference(typeDeclaration: TypeDeclaration): TypeReference = {
    TypeReference(
      typeName = typeDeclaration.typeName,
      genericTypes = typeDeclaration.genericTypes.map(toTypeReference)
    )
  }

  private case class Located[A](
    value: A,
    range: Range
  )

}