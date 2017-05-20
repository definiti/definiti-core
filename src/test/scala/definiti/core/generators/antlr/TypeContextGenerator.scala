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
    verifyingListContext <- Gen.option(VerificationContextGenerator.anyVerifyingListContext)
    genericTypeListContext <- Gen.option(GenericTypesContextGenerators.anyGenericTypeListContext)
    attributeDefinitionContexts <- Generators.listOfBoundedSize(0, 3, anyAttributeDefinitionContext)
    typeVerificationContexts <- Generators.listOfBoundedSize(0, 3, anyTypeVerificationContext)
  } yield {
    DefinedTypeContextMock(
      typeNameToken,
      identifier,
      docComment,
      verifyingListContext,
      genericTypeListContext,
      attributeDefinitionContexts,
      typeVerificationContexts
    )
  })

  lazy val definedTypeContextWithVerifyingList: Gen[DefinedTypeContext] = {
    anyDefinedTypeContext.filter(elt => isVerifyingListDefined(elt.verifyingList()))
  }

  lazy val anyAliasTypeContext: Gen[AliasTypeContext] = AntlrGenerator.genContext(for {
    typeNameToken <- AntlrGenerator.anyIdentifierToken
    genericTypesContext <- Gen.option(GenericTypesContextGenerators.anyGenericTypeListContext)
    referenceTypeNameToken <- AntlrGenerator.anyIdentifierToken
    aliasGenericTypesContext <- Gen.option(GenericTypesContextGenerators.anyGenericTypeListContext)
    identifiers <- Generators.listOfBoundedSize(0, 3, AntlrGenerator.anyIdentifierNode)
    docComment <- Gen.option(AntlrGenerator.anyDocCommentNode)
    verifyingListContext <- Gen.option(VerificationContextGenerator.anyVerifyingListContext)
  } yield {
    AliasTypeContextMock(
      typeNameToken,
      genericTypesContext,
      referenceTypeNameToken,
      aliasGenericTypesContext,
      identifiers,
      docComment,
      verifyingListContext
    )
  })

  lazy val aliasTypeContextWithVerifyingList: Gen[AliasTypeContext] = {
    anyAliasTypeContext.filter(elt => isVerifyingListDefined(elt.verifyingList()))
  }

  lazy val anyAttributeDefinitionContext: Gen[AttributeDefinitionContext] = AntlrGenerator.genContext(for {
    attributeNameToken <- AntlrGenerator.anyIdentifierToken
    attributeTypeToken <- AntlrGenerator.anyIdentifierToken
    verifyingListContext <- Gen.option(VerificationContextGenerator.anyVerifyingListContext)
    identifiers <- Generators.listOfBoundedSize(0, 3, AntlrGenerator.anyIdentifierNode)
    docComment <- Gen.option(AntlrGenerator.anyDocCommentNode)
    genericTypeListContext <- GenericTypesContextGenerators.anyGenericTypeListContext
  } yield {
    AttributeDefinitionContextMock(
      attributeNameToken,
      attributeTypeToken,
      verifyingListContext,
      identifiers,
      docComment,
      genericTypeListContext
    )
  })

  lazy val attributeDefinitionContextWithVerifyingList: Gen[AttributeDefinitionContext] = {
    anyAttributeDefinitionContext.filter(elt => isVerifyingListDefined(elt.verifyingList()))
  }

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

  lazy val anyTypeVerificationFunctionContext: Gen[TypeVerificationFunctionContext] = AntlrGenerator.genContext(for {
    identifier <- AntlrGenerator.anyIdentifierNode
    chainedExpressionContext <- ExpressionContextGenerator.anyChainedExpressionContext
  } yield {
    TypeVerificationFunctionContextMock(
      identifier,
      chainedExpressionContext
    )
  })

  private def isVerifyingListDefined(verifyingListContext: VerifyingListContext): Boolean = {
    verifyingListContext != null &&
      verifyingListContext.verifying() != null &&
      !verifyingListContext.verifying().isEmpty
  }
}
