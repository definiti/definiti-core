package definiti.core.generators.antlr

import definiti.core.generators.Generators
import definiti.core.mock.antlr._
import definiti.core.parser.antlr.DefinitiParser.{TypeVerificationFunctionContext, _}
import org.scalacheck.Gen

object TypeContextGenerator {
  lazy val anyDefinedTypeContext: Gen[DefinedTypeContext] = AntlrGenerator.genContext(for {
    typeNameToken <- AntlrGenerator.anyIdentifierToken
    identifier <- AntlrGenerator.anyIdentifierNode
    docComment <- Gen.option(AntlrGenerator.anyDocCommentNode)
    inheritances <- Generators.listOfBoundedSize(0, 3, anyInheritanceContext)
    genericTypeListContext <- Gen.option(GenericTypesContextGenerators.anyGenericTypeListContext)
    attributeDefinitionContexts <- Generators.listOfBoundedSize(0, 3, anyAttributeDefinitionContext)
    typeVerificationContexts <- Generators.listOfBoundedSize(0, 3, anyTypeVerificationContext)
  } yield {
    DefinedTypeContextMock(
      typeNameToken,
      identifier,
      docComment,
      inheritances,
      genericTypeListContext,
      attributeDefinitionContexts,
      typeVerificationContexts
    )
  })

  lazy val anyAliasTypeContext: Gen[AliasTypeContext] = AntlrGenerator.genContext(for {
    typeNameToken <- AntlrGenerator.anyIdentifierToken
    genericTypesContext <- Gen.option(GenericTypesContextGenerators.anyGenericTypeListContext)
    referenceTypeNameToken <- AntlrGenerator.anyIdentifierToken
    aliasGenericTypesContext <- Gen.option(GenericTypesContextGenerators.anyGenericTypeListContext)
    identifiers <- Generators.listOfBoundedSize(0, 3, AntlrGenerator.anyIdentifierNode)
    docComment <- Gen.option(AntlrGenerator.anyDocCommentNode)
    inheritances <- Generators.listOfBoundedSize(0, 3, anyInheritanceContext)
  } yield {
    AliasTypeContextMock(
      typeNameToken,
      genericTypesContext,
      referenceTypeNameToken,
      aliasGenericTypesContext,
      identifiers,
      docComment,
      inheritances
    )
  })

  lazy val anyInheritanceContext: Gen[InheritanceContext] = AntlrGenerator.genContext(for {
    verificationNameToken <- AntlrGenerator.anyIdentifierToken
    identifier <- AntlrGenerator.anyIdentifierNode
  } yield {
    InheritanceContextMock(
      verificationNameToken,
      identifier
    )
  })

  lazy val anyAttributeDefinitionContext: Gen[AttributeDefinitionContext] = AntlrGenerator.genContext(for {
    attributeNameToken <- AntlrGenerator.anyIdentifierToken
    attributeTypeToken <- AntlrGenerator.anyIdentifierToken
    attributeVerificationsContext <- anyAttributeVerificationContext
    identifiers <- Generators.listOfBoundedSize(0, 3, AntlrGenerator.anyIdentifierNode)
    docComment <- Gen.option(AntlrGenerator.anyDocCommentNode)
    genericTypeListContext <- GenericTypesContextGenerators.anyGenericTypeListContext
  } yield {
    AttributeDefinitionContextMock(
      attributeNameToken,
      attributeTypeToken,
      attributeVerificationsContext,
      identifiers,
      docComment,
      genericTypeListContext
    )
  })

  lazy val anyTypeVerificationContext: Gen[TypeVerificationContext] = AntlrGenerator.genContext(for {
    verificationMessageToken <- AntlrGenerator.anyStringToken
    typeVerificationFunctionContext <- anyTypeVerificationFunctionContext
    string <- AntlrGenerator.anyStringNode
  } yield {
    TypeVerificationContextMock(
      verificationMessageToken,
      typeVerificationFunctionContext,
      string
    )
  })

  lazy val anyAttributeVerificationContext: Gen[AttributeVerificationsContext] = AntlrGenerator.genContext(for {
    identifiers <- Generators.listOfBoundedSize(0, 3, AntlrGenerator.anyIdentifierNode)
  } yield {
    AttributeVerificationsContextMock(identifiers)
  })

  lazy val anyTypeVerificationFunctionContext: Gen[TypeVerificationFunctionContext] = AntlrGenerator.genContext(for {
    identifier <- AntlrGenerator.anyIdentifierNode
    chainedExpressionContext <- ExpressionContextGenerator.anyChainedExpressionContext
  } yield {
    TypeVerificationFunctionContextMock(
      identifier,
      chainedExpressionContext
    )
  })
}
