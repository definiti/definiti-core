package definiti.parser

import definiti._
import org.scalatest.{FlatSpec, Matchers}

class ExpressionParserSpec extends FlatSpec with Matchers {
  "The syntax enhancer" should "parse a string value expression" in {
    val input = Seq(QuotedString("My string"))
    val expected = QuotedStringExpressionToken("My string")

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  Seq(
    TrueKeyword -> true,
    FalseKeyword -> false
  ) foreach { case (token, boolean) =>
    it should s"parse a boolean value expression ($boolean)" in {
      val input = Seq(token)
      val expected = BooleanExpressionToken(boolean)

      ExpressionParser.buildExpression(input) should ===(expected)
    }
  }

  Seq[(String, BigDecimal)](
    "123" -> 123,
    "123.456" -> 123.456
  ) foreach { case (word, number) =>
    it should s"parse a number value expression ($word)" in {
      val input = Seq(Word(word))
      val expected = NumberExpressionToken(number)

      ExpressionParser.buildExpression(input) should ===(expected)
    }
  }

  it should "parse a variable expression" in {
    val input = Seq(Word("myVariable"))
    val expected = VariableExpressionToken("myVariable")

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should "parse an attribute call from variable" in {
    val input = Seq(Word("myVariable"), Dot, Word("myAttribute"))
    val expected = AttributeCallToken(VariableExpressionToken("myVariable"), "myAttribute")

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should "parse an attribute call from quoted string" in {
    val input = Seq(QuotedString("My string"), Dot, Word("myAttribute"))
    val expected = AttributeCallToken(QuotedStringExpressionToken("My string"), "myAttribute")

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should "parse an attribute call from boolean" in {
    val input = Seq(TrueKeyword, Dot, Word("myAttribute"))
    val expected = AttributeCallToken(BooleanExpressionToken(true), "myAttribute")

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should "parse an attribute call from number" in {
    val input = Seq(Word("123"), Dot, Word("myAttribute"))
    val expected = AttributeCallToken(NumberExpressionToken(123), "myAttribute")

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  Seq[(String, Seq[SyntaxToken], Seq[ExpressionToken])](
    ("no parameter", Seq(), Seq()),
    ("one parameter", Seq(Word("myParameter")), Seq(VariableExpressionToken("myParameter"))),
    ("two parameters", Seq(Word("myParameter"), Comma, QuotedString("My string")), Seq(VariableExpressionToken("myParameter"), QuotedStringExpressionToken("My string")))
  ) foreach { case (withParameterLabel, parameterTokens, parameterExpressions) =>
    it should s"parse a method call from variable with $withParameterLabel" in {
      val input = Seq(Word("myVariable"), Dot, Word("myMethod"), ParenthesisExpressionToken(parameterTokens))
      val expected = MethodCallToken(VariableExpressionToken("myVariable"), "myMethod", ListExpressionToken(parameterExpressions))

      ExpressionParser.buildExpression(input) should ===(expected)
    }

    it should s"parse a method call from quoted string with $withParameterLabel" in {
      val input = Seq(QuotedString("My string"), Dot, Word("myMethod"), ParenthesisExpressionToken(parameterTokens))
      val expected = MethodCallToken(QuotedStringExpressionToken("My string"), "myMethod", ListExpressionToken(parameterExpressions))

      ExpressionParser.buildExpression(input) should ===(expected)
    }

    it should s"parse a method call from boolean with $withParameterLabel" in {
      val input = Seq(FalseKeyword, Dot, Word("myMethod"), ParenthesisExpressionToken(parameterTokens))
      val expected = MethodCallToken(BooleanExpressionToken(false), "myMethod", ListExpressionToken(parameterExpressions))

      ExpressionParser.buildExpression(input) should ===(expected)
    }

    it should s"parse a method call from number with $withParameterLabel" in {
      val input = Seq(Word("123.456"), Dot, Word("myMethod"), ParenthesisExpressionToken(parameterTokens))
      val expected = MethodCallToken(NumberExpressionToken(123.456), "myMethod", ListExpressionToken(parameterExpressions))

      ExpressionParser.buildExpression(input) should ===(expected)
    }
  }

  it should "split a multi-expression with split by EOL" in {
    val input = Seq(
      Word("123"), Dot, Word("myAttribute"), EndOfLine,
      TrueKeyword
    )
    val expected = Seq(
      Seq(Word("123"), Dot, Word("myAttribute")),
      Seq(TrueKeyword)
    )

    ExpressionParser.splitExpressions(input) should ===(expected)
  }

  it should "split a multi-expression with EOL inside expressions" in {
    val input = Seq(
      FalseKeyword, EndOfLine, Dot, EndOfLine, Word("myMethod"), ParenthesisExpressionToken(Seq()), EndOfLine,
      Word("myVariable"), Dot, EndOfLine, Word("myAttribute"), EndOfLine,
      QuotedString("My string"), EndOfLine, Dot, Word("myOtherMethod"), ParenthesisExpressionToken(Seq(
        Word("myVariable"), Dot, EndOfLine, Word("myAttribute")
      ))
    )
    val expected = Seq(
      Seq(FalseKeyword, Dot, Word("myMethod"), ParenthesisExpressionToken(Seq())),
      Seq(Word("myVariable"), Dot, Word("myAttribute")),
      Seq(QuotedString("My string"), Dot, Word("myOtherMethod"), ParenthesisExpressionToken(Seq(
        Word("myVariable"), Dot, EndOfLine, Word("myAttribute")
      )))
    )

    ExpressionParser.splitExpressions(input) should ===(expected)
  }

  Seq[(String, Seq[SyntaxToken], ExpressionToken)](
    ("variable", Seq(Word("myVariable")), VariableExpressionToken("myVariable")),
    ("true", Seq(TrueKeyword), BooleanExpressionToken(true)),
    ("false", Seq(FalseKeyword), BooleanExpressionToken(false)),
    ("quoted string", Seq(QuotedString("My string")), QuotedStringExpressionToken("My string")),
    ("number", Seq(Word("123")), NumberExpressionToken(123)),
    ("attribute", Seq(Word("myVariable"), Dot, Word("myAttribute")), AttributeCallToken(VariableExpressionToken("myVariable"), "myAttribute")),
    ("method", Seq(Word("myVariable"), Dot, Word("myMethod"), ParenthesisExpressionToken(Seq())), MethodCallToken(VariableExpressionToken("myVariable"), "myMethod", ListExpressionToken(Seq())))
  ) foreach { case (typeOfExpression, contentOfExpression, expectedExpression) =>
    it should s"transform a parenthesis expression into non-parenthesis expression ($typeOfExpression)" in {
      val input = Seq(ParenthesisExpressionToken(contentOfExpression))
      val expected = expectedExpression

      ExpressionParser.buildExpression(input) should ===(expected)
    }
  }

  it should s"call an attribute from an expression token" in {
    val input = Seq(ParenthesisExpressionToken(Seq(Word("myVariable"))), Dot, Word("myAttribute"))
    val expected = AttributeCallToken(VariableExpressionToken("myVariable"), "myAttribute")

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should s"call a method from an expression token" in {
    val input = Seq(ParenthesisExpressionToken(Seq(Word("myVariable"))), Dot, Word("myMethod"), ParenthesisExpressionToken(Seq()))
    val expected = MethodCallToken(VariableExpressionToken("myVariable"), "myMethod", ListExpressionToken(Seq()))

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should s"parse a condition with no else" in {
    val input = Seq(
      ConditionToken(
        condition = ParenthesisExpressionToken(Seq(Word("myVariable"), Dot, Word("myAttribute"))),
        onTrue = BraceExpressionToken(Seq(Word("myOtherVariable"), Dot, Word("myMethod"), ParenthesisExpressionToken(Seq()))),
        onFalse = None
      )
    )
    val expected = ConditionExpressionToken(
      condition = AttributeCallToken(VariableExpressionToken("myVariable"), "myAttribute"),
      onTrue = MethodCallToken(VariableExpressionToken("myOtherVariable"), "myMethod", ListExpressionToken(Seq())),
      onFalse = None
    )

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should s"parse a condition with else" in {
    val input = Seq(
      ConditionToken(
        condition = ParenthesisExpressionToken(Seq(Word("myVariable"), Dot, Word("myAttribute"))),
        onTrue = BraceExpressionToken(Seq(Word("myOtherVariable"), Dot, Word("myMethod"), ParenthesisExpressionToken(Seq()))),
        onFalse = Some(BraceExpressionToken(Seq(Word("myOtherVariable"), Dot, Word("myOtherMethod"), ParenthesisExpressionToken(Seq()))))
      )
    )
    val expected = ConditionExpressionToken(
      condition = AttributeCallToken(VariableExpressionToken("myVariable"), "myAttribute"),
      onTrue = MethodCallToken(VariableExpressionToken("myOtherVariable"), "myMethod", ListExpressionToken(Seq())),
      onFalse = Some(MethodCallToken(VariableExpressionToken("myOtherVariable"), "myOtherMethod", ListExpressionToken(Seq())))
    )

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  Seq[(String, SyntaxToken, (ExpressionToken, ExpressionToken) => ExpressionToken)](
    ("||", OrSymbol, OrExpression.apply),
    ("&&", AndSymbol, AndExpression.apply),
    ("==", EqualSymbol, EqualExpression.apply),
    ("!=", NotEqualSymbol, NotEqualExpression.apply),
    ("<", LowerSymbol, LowerExpression.apply),
    (">", UpperSymbol, UpperExpression.apply),
    ("<=", LowerOrEqualSymbol, LowerOrEqualExpression.apply),
    (">=", UpperOrEqualSymbol, UpperOrEqualExpression.apply),
    ("+", PlusSymbol, PlusExpression.apply),
    ("-", MinusSymbol, MinusExpression.apply),
    ("*", TimeSymbol, TimeExpression.apply),
    ("/", DivideSymbol, DivideExpression.apply),
    ("%", ModuloSymbol, ModuloExpression.apply)
  ) foreach { case (symbol, syntaxToken, logicalExpressionApply) =>
    it should s"parse the logical expression $symbol" in {
      val leftInput = Seq(Word("myLeftVariable"))
      val rightInput = Seq(Word("123"), Dot, Word("myRightAttribute"))
      val input = leftInput ++ Seq(syntaxToken) ++ rightInput

      val leftExpected = VariableExpressionToken("myLeftVariable")
      val rightExpected = AttributeCallToken(NumberExpressionToken(123), "myRightAttribute")
      val expected = logicalExpressionApply(leftExpected, rightExpected)

      ExpressionParser.buildExpression(input) should ===(expected)
    }
  }

  it should "parse a not expression" in {
    val input = Seq(NotSymbol, Word("myVariable"), Dot, Word("myAttribute"))
    val expected = NotExpression(AttributeCallToken(VariableExpressionToken("myVariable"), "myAttribute"))

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should "parse a complex tree of symbols" in {
    val input = Seq(
      Word("now"), MinusSymbol, Word("before"),
      LowerSymbol,
      Word("duration"), Dot, Word("base"), PlusSymbol, Word("margin"), Dot, Word("max")
    )
    val expected = LowerExpression(
      MinusExpression(VariableExpressionToken("now"), VariableExpressionToken("before")),
      PlusExpression(
        AttributeCallToken(VariableExpressionToken("duration"), "base"),
        AttributeCallToken(VariableExpressionToken("margin"), "max")
      )
    )

    ExpressionParser.buildExpression(input) should ===(expected)
  }

  it should "parse chained calls of method and attributes" in {
    val input = Seq(
      QuotedString("My string"),
      Dot, Word("myAttribute"),
      Dot, Word("myMethod"), ParenthesisExpressionToken(Seq()),
      Dot, Word("myOtherAttribute"),
      Dot, Word("myFinalAttribute")
    )
    val expected = AttributeCallToken(
      AttributeCallToken(
        MethodCallToken(
          AttributeCallToken(
            QuotedStringExpressionToken("My string"),
            "myAttribute"
          ),
          "myMethod",
          ListExpressionToken(Seq())
        ),
        "myOtherAttribute"
      ),
      "myFinalAttribute"
    )

    ExpressionParser.buildExpression(input) should ===(expected)
  }
}
