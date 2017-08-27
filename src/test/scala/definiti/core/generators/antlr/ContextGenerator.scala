package definiti.core.generators.antlr

import definiti.core.mock.antlr.{ContextContentMock, ContextContentSymbolContextMock, ContextContextMock}
import definiti.core.parser.antlr.DefinitiParser._
import org.scalacheck.Gen

object ContextGenerator {
  lazy val anyContextContext: Gen[ContextContext] = AntlrGenerator.genContext(for {
    identifier <- AntlrGenerator.anyIdentifierNode
    contextContentContext <- anyContextContentContext
  } yield {
    ContextContextMock(
      identifier = identifier,
      contextContentContext = contextContentContext
    )
  })

  lazy val anyContextContentContext: Gen[ContextContentContext] = AntlrGenerator.genContext(for {
    contextContentSymbolContextSeq <- Gen.listOf(ContextContentSymbolContextMock)
  } yield {
    ContextContentMock(contextContentSymbolContextSeq)
  })

  def anyContextOf(name: String): Gen[ContextContext] = AntlrGenerator.genContext(for {
    identifier <- AntlrGenerator.anyIdentifierNodeWithText(name)
    contextContentContext <- anyContextContentContext
  } yield {
    ContextContextMock(
      identifier = identifier,
      contextContentContext = contextContentContext
    )
  })
}
