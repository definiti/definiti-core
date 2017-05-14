package definiti.core.generators.antlr

import definiti.core.mock.antlr.VerificationContextMock
import definiti.core.parser.antlr.DefinitiParser.VerificationContext
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
}
