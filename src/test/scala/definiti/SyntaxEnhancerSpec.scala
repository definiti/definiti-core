package definiti

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
      SymbolString("=>"),
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
        SymbolString("=>"),
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

  "SyntaxProcessor.buildFunctions" should "work" in {
    val input = Seq(
      Word("x"),
      SymbolString("="),
      ParenthesisExpressionToken(Seq(
        Word("string"),
        Colon,
        Word("String")
      )),
      MapSymbol,
      BraceExpressionToken(Seq(
        EndOfLine,
        QuotedString("The string is empty"),
        EndOfLine,
        ParenthesisExpressionToken(Seq(
          Word("string"),
          Colon,
          Word("String")
        )),
        MapSymbol,
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
      SymbolString("="),
      FunctionToken(
        Seq(FunctionParameter("string", "String")),
        BraceExpressionToken(Seq(
          EndOfLine,
          QuotedString("The string is empty"),
          EndOfLine,
          FunctionToken(
            Seq(FunctionParameter("string", "String")),
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
      FunctionParameter("string", "String"),
      FunctionParameter("min", "Int"),
      FunctionParameter("max", "Int")
    )
    val result = SyntaxEnhancer.extractParameterDefinition(input)

    result should ===(expected)
  }

  "SyntaxProcessor.buildConditions" should "work" in {
    val input = Seq(
      Word("x"),
      SymbolString("="),
      FunctionToken(
        Seq(FunctionParameter("string", "String")),
        BraceExpressionToken(Seq(
          EndOfLine,
          IfKeyword,
          ParenthesisExpressionToken(Seq(
            Word("x"),
            SymbolString("<"),
            Word("y")
          )),
          BraceExpressionToken(Seq(
            Word("x")
          )),
          EndOfLine,
          IfKeyword,
          ParenthesisExpressionToken(Seq(
            Word("x"),
            SymbolString("<="),
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
      SymbolString("="),
      FunctionToken(
        Seq(FunctionParameter("string", "String")),
        BraceExpressionToken(Seq(
          EndOfLine,
          ConditionToken(
            ParenthesisExpressionToken(Seq(
              Word("x"),
              SymbolString("<"),
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
              SymbolString("<="),
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
}
