package definiti

import org.scalatest.{FlatSpec, Matchers}

class SyntaxProcessorSpec extends FlatSpec with Matchers {
  "SyntaxProcessor.process" should "work" in {
    val input =
      """
        |verification NonEmpty {
        |  "The string is empty"
        |  (string: String) => string.nonEmpty()
        |}
      """.stripMargin
    val expected = Seq(
      EndOfLine,
      VerificationKeyword,
      Word("NonEmpty"),
      OpenBrace,
      EndOfLine,
      QuotedString("The string is empty"),
      EndOfLine,
      OpenParenthesis,
      Word("string"),
      Colon,
      Word("String"),
      CloseParenthesis,
      MapSymbol,
      Word("string"),
      Dot,
      Word("nonEmpty"),
      OpenParenthesis,
      CloseParenthesis,
      EndOfLine,
      CloseBrace,
      EndOfLine
    )
    val result = SyntaxProcessor.processString(input)

    result should ===(expected)
  }
}
