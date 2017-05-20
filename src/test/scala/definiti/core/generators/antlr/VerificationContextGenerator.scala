package definiti.core.generators.antlr

import definiti.core.mock.antlr.{VerificationContextMock, VerifyingContextMock, VerifyingListContextMock}
import definiti.core.parser.antlr.DefinitiParser.{VerificationContext, VerifyingContext, VerifyingListContext}
import org.scalacheck.Gen

object VerificationContextGenerator {
  lazy val anyVerificationContext: Gen[VerificationContext] = AntlrGenerator.genContext(for {
    verificationNameToken <- AntlrGenerator.anyIdentifierToken
    verificationMessageToken <- AntlrGenerator.anyStringToken
    functionContext <- FunctionContextGenerator.anyFunctionContext
    identifier <- AntlrGenerator.anyIdentifierNode
    string <- AntlrGenerator.anyStringNode
    docComment <- AntlrGenerator.anyDocCommentNode
  } yield {
    VerificationContextMock(
      verificationNameToken,
      verificationMessageToken,
      functionContext,
      identifier,
      string,
      docComment
    )
  })

  lazy val anyVerifyingListContext: Gen[VerifyingListContext] = AntlrGenerator.genContext(for {
    verifyingContexts <- Gen.listOf(anyVerifyingContext)
  } yield {
    VerifyingListContextMock(verifyingContexts)
  })

  lazy val verifyingListContextWithMessage: Gen[VerifyingListContext] = AntlrGenerator.genContext(for {
    verifyingContexts <- Gen.listOf(verifyingContextWithMessage)
  } yield {
    VerifyingListContextMock(verifyingContexts)
  })

  lazy val verifyingListContextWithoutMessage: Gen[VerifyingListContext] = AntlrGenerator.genContext(for {
    verifyingContexts <- Gen.listOf(verifyingContextWithoutMessage)
  } yield {
    VerifyingListContextMock(verifyingContexts)
  })

  lazy val anyVerifyingContext: Gen[VerifyingContext] = AntlrGenerator.genContext(for {
    verificationNameToken <- AntlrGenerator.anyIdentifierToken
    messageToken <- Gen.option(AntlrGenerator.anyStringToken)
  } yield {
    VerifyingContextMock(
      verificationNameToken,
      messageToken
    )
  })

  lazy val verifyingContextWithMessage: Gen[VerifyingContext] = AntlrGenerator.genContext(for {
    verificationNameToken <- AntlrGenerator.anyIdentifierToken
    messageToken <- AntlrGenerator.anyStringToken
  } yield {
    VerifyingContextMock(
      verificationNameToken,
      Some(messageToken)
    )
  })

  lazy val verifyingContextWithoutMessage: Gen[VerifyingContext] = AntlrGenerator.genContext(for {
    verificationNameToken <- AntlrGenerator.anyIdentifierToken
  } yield {
    VerifyingContextMock(
      verificationNameToken,
      None
    )
  })
}
