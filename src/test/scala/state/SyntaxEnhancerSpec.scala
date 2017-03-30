package state

import org.scalatest.{FlatSpec, Matchers}

class SyntaxEnhancerSpec extends FlatSpec with Matchers {
  "SyntaxProcessor.buildEnclosing" should "work" in {
    val input = Seq(
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
      Symbol("=>"),
      Word("string"),
      Dot,
      Word("nonEmpty"),
      OpenParenthesis,
      CloseParenthesis,
      EndOfLine,
      CloseBrace,
      EndOfLine
    )
    val expected = Seq(
      EndOfLine,
      VerificationKeyword,
      Word("NonEmpty"),
      BraceExpressionSyntax(Seq(
        EndOfLine,
        QuotedString("The string is empty"),
        EndOfLine,
        ParenthesisExpressionSyntax(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        Word("string"),
        Dot,
        Word("nonEmpty"),
        ParenthesisExpressionSyntax(Seq()),
        EndOfLine
      )),
      EndOfLine
    )
    val result = SyntaxEnhancer.buildEnclosing(input)

    result should ===(expected)
  }

  "SyntaxProcessor.trimEOL" should "work" in {
    val input = Seq(
      EndOfLine,
      EndOfLine,
      Word("NonEmpty"),
      EndOfLine,
      QuotedString("The string is empty"),
      EndOfLine,
      OpenParenthesis,
      CloseParenthesis,
      EndOfLine,
      EndOfLine
    )
    val expected = Seq(
      Word("NonEmpty"),
      EndOfLine,
      QuotedString("The string is empty"),
      EndOfLine,
      OpenParenthesis,
      CloseParenthesis
    )
    val result = SyntaxEnhancer.trimEOL(input)

    result should ===(expected)
  }

  "SyntaxProcessor.trimEOLAroundEncloser" should "work" in {
    val input = Seq(
      EndOfLine,
      VerificationKeyword,
      Word("NonEmpty"),
      BraceExpressionSyntax(Seq(
        EndOfLine,
        QuotedString("The string is empty"),
        EndOfLine,
        ParenthesisExpressionSyntax(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        Word("string"),
        Dot,
        Word("nonEmpty"),
        ParenthesisExpressionSyntax(Seq()),
        EndOfLine
      )),
      EndOfLine
    )
    val expected = Seq(
      VerificationKeyword,
      Word("NonEmpty"),
      BraceExpressionSyntax(Seq(
        QuotedString("The string is empty"),
        ParenthesisExpressionSyntax(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        Word("string"),
        Dot,
        Word("nonEmpty"),
        ParenthesisExpressionSyntax(Seq())
      ))
    )
    val result = SyntaxEnhancer.trimEOLAroundEncloser(input)

    result should ===(expected)
  }

  "SyntaxProcessor.buildFunctions" should "work" in {
    val input = Seq(
      Word("x"),
      Symbol("="),
      ParenthesisExpressionSyntax(Seq(
        Word("string"),
        Colon,
        Word("String")
      )),
      Symbol("=>"),
      BraceExpressionSyntax(Seq(
        EndOfLine,
        QuotedString("The string is empty"),
        EndOfLine,
        ParenthesisExpressionSyntax(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        BraceExpressionSyntax(Seq(
          Word("string"),
          Dot,
          Word("nonEmpty"),
          ParenthesisExpressionSyntax(Seq()),
          EndOfLine
        ))
      )),
      EndOfLine
    )
    val expected = Seq(
      Word("x"),
      Symbol("="),
      FunctionSyntax(
        ParenthesisExpressionSyntax(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        BraceExpressionSyntax(Seq(
          EndOfLine,
          QuotedString("The string is empty"),
          EndOfLine,
          FunctionSyntax(
            ParenthesisExpressionSyntax(Seq(
              Word("string"),
              Colon,
              Word("String")
            )),
            BraceExpressionSyntax(Seq(
              Word("string"),
              Dot,
              Word("nonEmpty"),
              ParenthesisExpressionSyntax(Seq()),
              EndOfLine
            ))
          )
        ))
      ),
      EndOfLine
    )
    val result = SyntaxEnhancer.buildFunctions(input)

    result should ===(expected)
  }
}
