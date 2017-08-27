package definiti.core.generators.antlr

import definiti.core.generators.Generators
import definiti.core.mock.antlr.{TerminalNodeMock, TokenMock}
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.scalacheck.Gen

object AntlrGenerator {
  lazy val anyBooleanNode: Gen[TerminalNode] = genNode(anyBooleanToken)
  lazy val anyBooleanToken: Gen[Token] = genToken(Generators.anyBooleanText)

  lazy val anyNumberNode: Gen[TerminalNode] = genNode(anyNumberToken)
  lazy val anyNumberToken: Gen[Token] = genToken(Generators.numberAsString)

  lazy val anyStringNode: Gen[TerminalNode] = genNode(anyStringToken)
  lazy val anyStringToken: Gen[Token] = genToken(Generators.anyString.map('"' + _ + '"'))

  lazy val anyIdentifierNode: Gen[TerminalNode] = genNode(anyIdentifierToken)
  lazy val anyIdentifierToken: Gen[Token] = genToken(Generators.anyIdentifier)

  def anyIdentifierNodeWithText(text: String): Gen[TerminalNode] = genNode(anyIdentifierTokenWithText(text))
  def anyIdentifierTokenWithText(text: String): Gen[Token] = genToken(Gen.const(text))

  lazy val anyBinaryOperatorNode: Gen[TerminalNode] = genNode(anyBinaryOperatorToken)
  lazy val anyBinaryOperatorToken: Gen[Token] = genToken(Gen.oneOf("*", "/", "%", "+", "-", "==", "!=", "<", "<=", ">", ">=", "&&", "||"))

  lazy val anyNotOperatorNode: Gen[TerminalNode] = genNode(anyNotOperatorToken)
  lazy val anyNotOperatorToken: Gen[Token] = genToken(Gen.const("!"))

  lazy val anyDocCommentNode: Gen[TerminalNode] = genNode(anyDocCommentToken)
  lazy val anyDocCommentToken: Gen[Token] = genToken(Generators.anyString.map("/**" + _ + "*/"))

  def genContext[A <: ParserRuleContext](gen: Gen[A]): Gen[A] = for {
    element <- gen
    startToken <- anyIdentifierToken
    stopToken <- anyIdentifierToken
  } yield {
    element.start = startToken
    element.stop = stopToken
    element
  }

  private def genNode(genToken: Gen[Token]): Gen[TerminalNode] = genToken.map(TerminalNodeMock(_))

  private def genToken(genText: Gen[String]): Gen[Token] = for {
    text <- genText
    line <- Gen.posNum[Int]
    startIndex <- Gen.posNum[Int]
    stopIndex <- Gen.posNum[Int]
  } yield {
    TokenMock(text, line, startIndex, stopIndex)
  }
}
