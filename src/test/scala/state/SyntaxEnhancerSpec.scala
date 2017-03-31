package state

import org.scalatest.{FlatSpec, Matchers}

class SyntaxEnhancerSpec extends FlatSpec with Matchers {
  "SyntaxProcessor.squashEOL" should "work" in {
    val input = Seq(
      EndOfLine,
      EndOfLine,
      VerificationKeyword,
      Word("NonEmpty"),
      OpenBrace,
      EndOfLine,
      EndOfLine,
      QuotedString("The string is empty"),
      EndOfLine,
      EndOfLine,
      EndOfLine,
      EndOfLine,
      EndOfLine,
      EndOfLine,
      EndOfLine,
      EndOfLine,
      OpenParenthesis,
      Word("string"),
      Colon,
      EndOfLine,
      EndOfLine,
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
      EndOfLine,
      EndOfLine,
      EndOfLine
    )
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
      EndOfLine,
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
    val result = SyntaxEnhancer.squashEOL(input)

    result should ===(expected)
  }

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

  "SyntaxProcessor.buildConditions" should "work" in {
    val input = Seq(
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
          IfKeyword,
          ParenthesisExpressionToken(Seq(
            Word("x"),
            Symbol("<"),
            Word("y")
          )),
          BraceExpressionToken(Seq(
            Word("x")
          )),
          EndOfLine,
          IfKeyword,
          ParenthesisExpressionToken(Seq(
            Word("x"),
            Symbol("<="),
            Word("1")
          )),
          BraceExpressionToken(Seq(
            Word("x")
          )),
          EndOfLine,
          ElseKeyword,
          BraceExpressionToken(Seq(
            Word("y")
          ))
        ))
      ),
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
          ConditionToken(
            ParenthesisExpressionToken(Seq(
              Word("x"),
              Symbol("<"),
              Word("y")
            )),
            BraceExpressionToken(Seq(
              Word("x")
            )),
            None
          ),
          EndOfLine,
          ConditionToken(
            ParenthesisExpressionToken(Seq(
              Word("x"),
              Symbol("<="),
              Word("1")
            )),
            BraceExpressionToken(Seq(
              Word("x")
            )),
            Some(BraceExpressionToken(Seq(
              Word("y")
            )))
          )
        ))
      ),
      EndOfLine
    )
    val result = SyntaxEnhancer.buildConditions(input)

    result should ===(expected)
  }

  "SyntaxProcessor.buildMethodOrAttributeCall" should "work" in {
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
        EndOfLine,
        Word("string"),
        EndOfLine,
        Dot,
        EndOfLine,
        Word("length"),
        EndOfLine
      )),
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
        MethodCall("string", "nonEmpty", ParenthesisExpressionToken(Seq())),
        EndOfLine,
        AttributeCall("string", "length"),
        EndOfLine
      )),
      EndOfLine
    )
    val result = SyntaxEnhancer.buildMethodOrAttributeCall(input)

    result should ===(expected)
  }
}
