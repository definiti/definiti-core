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
      BraceExpressionToken(Seq(
        EndOfLine,
        QuotedString("The string is empty"),
        EndOfLine,
        ParenthesisExpressionToken(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        Word("string"),
        Dot,
        Word("nonEmpty"),
        ParenthesisExpressionToken(Seq()),
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
      BraceExpressionToken(Seq(
        EndOfLine,
        QuotedString("The string is empty"),
        EndOfLine,
        ParenthesisExpressionToken(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        Word("string"),
        Dot,
        Word("nonEmpty"),
        ParenthesisExpressionToken(Seq()),
        EndOfLine
      )),
      EndOfLine
    )
    val expected = Seq(
      VerificationKeyword,
      Word("NonEmpty"),
      BraceExpressionToken(Seq(
        QuotedString("The string is empty"),
        ParenthesisExpressionToken(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        Word("string"),
        Dot,
        Word("nonEmpty"),
        ParenthesisExpressionToken(Seq())
      ))
    )
    val result = SyntaxEnhancer.trimEOLAroundEncloser(input)

    result should ===(expected)
  }

  "SyntaxProcessor.buildFunctions" should "work" in {
    val input = Seq(
      Word("x"),
      Symbol("="),
      ParenthesisExpressionToken(Seq(
        Word("string"),
        Colon,
        Word("String")
      )),
      Symbol("=>"),
      BraceExpressionToken(Seq(
        EndOfLine,
        QuotedString("The string is empty"),
        EndOfLine,
        ParenthesisExpressionToken(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        Symbol("=>"),
        BraceExpressionToken(Seq(
          Word("string"),
          Dot,
          Word("nonEmpty"),
          ParenthesisExpressionToken(Seq()),
          EndOfLine
        ))
      )),
      EndOfLine
    )
    val expected = Seq(
      Word("x"),
      Symbol("="),
      FunctionToken(
        ParenthesisExpressionToken(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        BraceExpressionToken(Seq(
          EndOfLine,
          QuotedString("The string is empty"),
          EndOfLine,
          FunctionToken(
            ParenthesisExpressionToken(Seq(
              Word("string"),
              Colon,
              Word("String")
            )),
            BraceExpressionToken(Seq(
              Word("string"),
              Dot,
              Word("nonEmpty"),
              ParenthesisExpressionToken(Seq()),
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

  "SyntaxProcessor.extractParameterDefinition" should "work" in {
    val input = ParenthesisExpressionToken(Seq(
      Word("string"),
      Colon,
      Word("String"),
      Comma,
      Word("min"),
      Colon,
      Word("Int"),
      Comma,
      Word("max"),
      Colon,
      Word("Int")
    ))
    val expected = Seq(
      Parameter("string", "String"),
      Parameter("min", "Int"),
      Parameter("max", "Int")
    )
    val result = SyntaxEnhancer.extractParameterDefinition(input)

    result should ===(expected)
  }
}
