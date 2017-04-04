package definiti

import org.scalatest.{FlatSpec, Matchers}

class SyntaxProcessorFullSpec extends FlatSpec with Matchers {
  "SyntaxProcessor.process" should "work" in {
    val input =
      """
        |verification NonEmpty {
        |  "The string is empty // quoted comment"
        |  (string: String) => { string.nonEmpty() }
        |}
        |""".stripMargin + /* Force insertion of space not cleared with autoformat */ " " + """
        |verification NonBlank {
        |  "The string is blank /* quoted comment */"
        |  (string: String) => { string.trim().nonEmpty() }
        |}
        |
        |/*
        |  Could be simplified, but it is for the "example"
        |*/
        |verification PhoneNumber {
        |  "Please provide a phone number"
        |  (string: String) => {
        |    if (string.nonEmpty()) {
        |      if (string.startsWith("+33")) {
        |        string.matches("^\+33\d{9}$")
        |      } else {
        |        string.matches("^0\d{9}$")
        |      }
        |    } else {
        |      false
        |    }
        |  }
        |}
        |
        |type Period {
        |  start: Date
        |  end: Date
        |
        |  verify {
        |    "end should be after start"
        |    (period: Period) => { end > start || end == start }
        |  }
        |}
        |
        |verification YearPeriod {
        |  "The period must last one year"
        |  (period: Period) => {
        |    // Not quite "right" but show the idea
        |    // hypothese: timestamp in seconds
        |    period.end.timestamp - period.start.timestamp == 365*24*3600
        |  }
        |}
        |
        |verification StartJanuaryFirst {
        |  "The period must start on january the first"
        |  (period: Period) => { period.start.day == 1 && period.start.month == 1 }
        |}
        |
        |type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
      """.stripMargin
    val expected = Seq(
      EndOfLine,
      // verification NonEmpty {
      VerificationKeyword, Word("NonEmpty"), OpenBrace, EndOfLine,
      //   "The string is empty // quoted comment"
      QuotedString("The string is empty // quoted comment"), EndOfLine,
      //   (string: String) => { string.nonEmpty() }
      OpenParenthesis, Word("string"), Colon, Word("String"), CloseParenthesis, MapSymbol,
      OpenBrace, Word("string"), Dot, Word("nonEmpty"), OpenParenthesis, CloseParenthesis, CloseBrace, EndOfLine,
      // }
      CloseBrace, EndOfLine,
      // verification NonBlank {
      VerificationKeyword, Word("NonBlank"), OpenBrace, EndOfLine,
      //   "The string is blank /* quoted comment */"
      QuotedString("The string is blank /* quoted comment */"), EndOfLine,
      //   (string: String) => { string.trim().nonEmpty() }
      OpenParenthesis, Word("string"), Colon, Word("String"), CloseParenthesis, MapSymbol,
      OpenBrace, Word("string"), Dot, Word("trim"), OpenParenthesis, CloseParenthesis,
      Dot, Word("nonEmpty"), OpenParenthesis, CloseParenthesis, CloseBrace, EndOfLine,
      // }
      CloseBrace, EndOfLine,
      // /* \n Could be simplified, but it is for the "example" \n*/
      BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"), EndOfLine,
      // verification PhoneNumber {
      VerificationKeyword, Word("PhoneNumber"), OpenBrace, EndOfLine,
      //   "Please provide a phone number"
      QuotedString("Please provide a phone number"), EndOfLine,
      //   (string: String) => {
      OpenParenthesis, Word("string"), Colon, Word("String"), CloseParenthesis, MapSymbol, OpenBrace, EndOfLine,
      //     if (string.nonEmpty()) {
      IfKeyword, OpenParenthesis, Word("string"), Dot, Word("nonEmpty"), OpenParenthesis, CloseParenthesis, CloseParenthesis, OpenBrace, EndOfLine,
      //       if (string.startsWith("+33")) {
      IfKeyword, OpenParenthesis, Word("string"), Dot, Word("startsWith"), OpenParenthesis, QuotedString("+33"), CloseParenthesis, CloseParenthesis, OpenBrace, EndOfLine,
      //         string.matches("^\+33\d{9}$")
      Word("string"), Dot, Word("matches"), OpenParenthesis, QuotedString("^\\+33\\d{9}$"), CloseParenthesis, EndOfLine,
      //       } else {
      CloseBrace, ElseKeyword, OpenBrace, EndOfLine,
      //         string.matches("^0\d{9}$")
      Word("string"), Dot, Word("matches"), OpenParenthesis, QuotedString("^0\\d{9}$"), CloseParenthesis, EndOfLine,
      //       }
      CloseBrace, EndOfLine,
      //     } else {
      CloseBrace, ElseKeyword, OpenBrace, EndOfLine,
      //       false
      FalseKeyword, EndOfLine,
      //     }
      CloseBrace, EndOfLine,
      //   }
      CloseBrace, EndOfLine,
      // }
      CloseBrace, EndOfLine,
      // type Period {
      TypeKeyword, Word("Period"), OpenBrace, EndOfLine,
      //   start: Date
      Word("start"), Colon, Word("Date"), EndOfLine,
      //   end: Date
      Word("end"), Colon, Word("Date"), EndOfLine,
      //   verify {
      VerifyKeyword, OpenBrace, EndOfLine,
      //     "end should be after start"
      QuotedString("end should be after start"), EndOfLine,
      //     (period: Period) => { end > start || end == start }
      OpenParenthesis, Word("period"), Colon, Word("Period"), CloseParenthesis, MapSymbol,
      OpenBrace, Word("end"), UpperSymbol, Word("start"), OrSymbol,
      Word("end"), EqualSymbol, Word("start"), CloseBrace, EndOfLine,
      //   }
      CloseBrace, EndOfLine,
      // }
      CloseBrace, EndOfLine,
      // verification YearPeriod {
      VerificationKeyword, Word("YearPeriod"), OpenBrace, EndOfLine,
      //   "The period must last one year"
      QuotedString("The period must last one year"), EndOfLine,
      //   (period: Period) => {
      OpenParenthesis, Word("period"), Colon, Word("Period"), CloseParenthesis, MapSymbol, OpenBrace, EndOfLine,
      //     // Not quite "right" but show the idea
      LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
      //     // hypothese: timestamp in seconds
      LineComment(" hypothese: timestamp in seconds"), EndOfLine,
      //     period.end.timestamp - period.start.timestamp == 365*24*3600
      Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
      Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
      Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600"), EndOfLine,
      //   }
      CloseBrace, EndOfLine,
      // }
      CloseBrace, EndOfLine,
      // verification StartJanuaryFirst {
      VerificationKeyword, Word("StartJanuaryFirst"), OpenBrace, EndOfLine,
      //   "The period must start on january the first"
      QuotedString("The period must start on january the first"), EndOfLine,
      //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
      OpenParenthesis, Word("period"), Colon, Word("Period"), CloseParenthesis, MapSymbol,
      OpenBrace, Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
      Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1"), CloseBrace, EndOfLine,
      // }
      CloseBrace, EndOfLine,
      // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
      TypeKeyword, Word("CivilYear"), AssignSymbol, Word("Period"), VerifyingKeyword, Word("YearPeriod"),
      VerifyingKeyword, Word("StartJanuaryFirst"), EndOfLine
    )
    val result = SyntaxProcessor.processString(input)

    result should ===(expected)
  }
}
