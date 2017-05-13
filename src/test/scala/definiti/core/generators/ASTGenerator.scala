package definiti.core.generators

import definiti.core.{AttributeDefinition, Context, DefinedFunction, ParameterDefinition, Position, Range, ReferenceContext, TypeReference}
import org.scalacheck.{Arbitrary, Gen}

import scala.annotation.tailrec

object ASTGenerator {
  def anyFunction(implicit context: Context): Gen[DefinedFunction] = for {
    parameters <- Gen.listOf(anyParameterDefinition)
    body <- ExpressionGenerator.anyExpression
    genericTypes <- Gen.listOf(anyIdentifier)
    range <- anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  // We do not generate recursive ParameterDefinition (eg: List[Option[List[String]]]) because of possible stack overflows
  def anyParameterDefinition(implicit context: Context): Gen[ParameterDefinition] = for {
    name <- anyIdentifier
    typeReference <- anyTypeReference
    range <- anyRange
  } yield {
    ParameterDefinition(
      name = name,
      typeReference = typeReference,
      range = range
    )
  }

  def anyTypeReference(implicit context: Context): Gen[TypeReference] = for {
    typeName <- anyIdentifier
  } yield {
    TypeReference(
      typeName = typeName,
      genericTypes = Seq.empty
    )
  }

  def referencedTypeReference(implicit context: ReferenceContext): Gen[TypeReference] = {
    def process(n: Int): Gen[TypeReference] = n match {
      case 0 =>
        for {
          classDefinition <- Gen.oneOf(context.classes)
        } yield {
          TypeReference(
            typeName = classDefinition.canonicalName,
            genericTypes = classDefinition.genericTypes.map(_ => TypeReference(typeName = "Boolean", genericTypes = Seq.empty))
          )
        }
      case other if other > 0 =>
        for {
          classDefinition <- Gen.oneOf(context.classes)
          genericTypes <- Gen.listOfN(classDefinition.genericTypes.length, process(other - 1))
        } yield {
          TypeReference(
            typeName = classDefinition.canonicalName,
            genericTypes = genericTypes
          )
        }
    }

    process(2)
  }

  def anyAttributeDefinition(implicit context: ReferenceContext): Gen[AttributeDefinition] = for {
    name <- anyIdentifier
    typeReference <- anyTypeReference
    comment <- Gen.option(anyIdentifier)
    verifications <- Gen.listOf(anyIdentifier)
    range <- anyRange
  } yield {
    AttributeDefinition(
      name,
      typeReference,
      comment,
      verifications,
      range
    )
  }

  def referencedAttributeDefinition(implicit context: ReferenceContext): Gen[AttributeDefinition] = for {
    attributeDefinition <- anyAttributeDefinition
    typeReference <- referencedTypeReference
    verifications <- Gen.listOf(referencedVerificationIdentifier)
  } yield {
    attributeDefinition.copy(
      typeReference = typeReference,
      verifications = verifications
    )
  }

  def attributeDefinitionWithNonReferencedType(implicit context: ReferenceContext): Gen[AttributeDefinition] = for {
    attributeDefinition <- anyAttributeDefinition
  } yield {
    attributeDefinition.copy(
      typeReference = makeNonReferenced(attributeDefinition.typeReference)
    )
  }

  def attributeDefinitionWithNonReferencedVerification(implicit context: ReferenceContext): Gen[AttributeDefinition] = for {
    attributeDefinition <- anyAttributeDefinition
    verificationName <- anyIdentifier
  } yield {
    attributeDefinition.copy(
      verifications = attributeDefinition.verifications :+ makeNonReferenced(verificationName)
    )
  }

  def referencedVerificationIdentifier(implicit context: ReferenceContext): Gen[String] = for {
    verification <- Gen.oneOf(context.verifications)
  } yield {
    verification.canonicalName
  }

  def anyIdentifier(implicit context: Context): Gen[String] = Gen.alphaNumStr

  def anyPackageName(implicit context: Context): Gen[String] = Gen.listOf(Gen.frequency((1, '.'), (10, Gen.alphaNumChar))).map(_.mkString)

  def anyString(implicit context: Context): Gen[String] = Arbitrary.arbString.arbitrary

  def anyRange(implicit context: Context): Gen[Range] = for {
    firstLine <- Gen.choose(0, Int.MaxValue)
    secondLine <- Gen.choose(0, Int.MaxValue)
    firstColumn <- Gen.choose(0, Int.MaxValue)
    secondColumn <- Gen.choose(0, Int.MaxValue)
  } yield {
    val lineStart = Math.min(firstLine, secondLine)
    val lineEnd = Math.max(firstLine, secondLine)
    val columnStart = if (lineStart == lineEnd) Math.min(firstColumn, secondColumn) else firstColumn
    val columnEnd = if (lineStart == lineEnd) Math.max(firstColumn, secondColumn) else secondColumn
    Range(
      start = Position(line = lineStart, column = columnStart),
      end = Position(line = lineEnd, column = columnEnd)
    )
  }

  def listOfGenericTypeDefinition(implicit context: Context): Gen[Seq[String]] = {
    Generators.listOfBoundedSize(0, 2, ASTGenerator.anyIdentifier)
  }

  def listOfGenericType(implicit context: Context): Gen[Seq[TypeReference]] = {
    Generators.listOfBoundedSize(0, 2, ASTGenerator.anyTypeReference)
  }

  def makeNonReferenced(name: String)(implicit context: ReferenceContext): String = {
    val forbiddenNames = context.verifications.map(_.canonicalName) ++ context.classes.map(_.canonicalName)
    @tailrec def process(currentName: String): String = {
      if (forbiddenNames.contains(currentName)) {
        process(currentName + Gen.alphaNumChar.sample.getOrElse('a'))
      } else {
        currentName
      }
    }
    process(name)
  }

  def makeNonReferenced(typeReference: TypeReference)(implicit context: ReferenceContext): TypeReference = {
    typeReference.copy(
      typeName = makeNonReferenced(typeReference.typeName)
    )
  }
}