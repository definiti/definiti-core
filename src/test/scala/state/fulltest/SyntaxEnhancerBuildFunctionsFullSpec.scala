package state.fulltest

import org.scalatest.{FlatSpec, Matchers}
import state._

class SyntaxEnhancerBuildFunctionsFullSpec extends FlatSpec with Matchers {
  "SyntaxEnhancer.buildEnclosing" should "work" in {
    val input = fulltest.inputWithEnclosing
    val expected = Seq(
      EndOfLine,
      // verification NonEmpty {
      VerificationKeyword, Word("NonEmpty"), BraceExpressionToken(Seq(EndOfLine,
        //   "The string is empty // quoted comment"
        QuotedString("The string is empty // quoted comment"), EndOfLine,
        //   (string: String) => { string.nonEmpty() }
        FunctionToken(
          ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
          BraceExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())))
        ), EndOfLine
        // }
      )), EndOfLine,
      // verification NonBlank {
      VerificationKeyword, Word("NonBlank"), BraceExpressionToken(Seq(EndOfLine,
        //   "The string is blank /* quoted comment */"
        QuotedString("The string is blank /* quoted comment */"), EndOfLine,
        //   (string: String) => { string.trim().nonEmpty() }
        FunctionToken(
          ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
          BraceExpressionToken(Seq(
            Word("string"), Dot, Word("trim"), ParenthesisExpressionToken(Seq()),
            Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())
          ))
        ), EndOfLine
        // }
      )), EndOfLine,
      // /* \n Could be simplified, but it is for the "example" \n*/
      BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"), EndOfLine,
      // verification PhoneNumber {
      VerificationKeyword, Word("PhoneNumber"), BraceExpressionToken(Seq(EndOfLine,
        //   "Please provide a phone number"
        QuotedString("Please provide a phone number"), EndOfLine,
        //   (string: String) => {
        FunctionToken(
          ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
          BraceExpressionToken(Seq(EndOfLine,
            //     if (string.nonEmpty()) {
            IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq()))), BraceExpressionToken(Seq(EndOfLine,
              //       if (string.startsWith("+33")) {
              IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("startsWith"), ParenthesisExpressionToken(Seq(QuotedString("+33"))))), BraceExpressionToken(Seq(EndOfLine,
                //         string.matches("^\+33\d{9}$")
                Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^\\+33\\d{9}$"))), EndOfLine
                //       } else {
              )), ElseKeyword, BraceExpressionToken(Seq(EndOfLine,
                //         string.matches("^0\d{9}$")
                Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^0\\d{9}$"))), EndOfLine
                //       }
              )), EndOfLine
              //     } else {
            )), ElseKeyword, BraceExpressionToken(Seq(EndOfLine,
              //       false
              FalseKeyword, EndOfLine
              //     }
            )), EndOfLine
            //   }
          ))
        ), EndOfLine
        // }
      )), EndOfLine,
      // type Period {
      TypeKeyword, Word("Period"), BraceExpressionToken(Seq(EndOfLine,
        //   start: Date
        Word("start"), Colon, Word("Date"), EndOfLine,
        //   end: Date
        Word("end"), Colon, Word("Date"), EndOfLine,
        //   verify {
        VerifyKeyword, BraceExpressionToken(Seq(EndOfLine,
          //     "end should be after start"
          QuotedString("end should be after start"), EndOfLine,
          //     (period: Period) => { end > start || end == start }
          FunctionToken(
            ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
            BraceExpressionToken(Seq(Word("end"), UpperSymbol, Word("start"), OrSymbol,
              Word("end"), EqualSymbol, Word("start")))
          ), EndOfLine
          //   }
        )), EndOfLine
        // }
      )), EndOfLine,
      // verification YearPeriod {
      VerificationKeyword, Word("YearPeriod"), BraceExpressionToken(Seq(EndOfLine,
        //   "The period must last one year"
        QuotedString("The period must last one year"), EndOfLine,
        //   (period: Period) => {
        FunctionToken(
          ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
          BraceExpressionToken(Seq(EndOfLine,
            //     // Not quite "right" but show the idea
            LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
            //     // hypothese: timestamp in seconds
            LineComment(" hypothese: timestamp in seconds"), EndOfLine,
            //     period.end.timestamp - period.start.timestamp == 365*24*3600
            Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
            Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
            Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600"), EndOfLine
            //   }
          ))
        ), EndOfLine
        // }
      )), EndOfLine,
      // verification StartJanuaryFirst {
      VerificationKeyword, Word("StartJanuaryFirst"), BraceExpressionToken(Seq(EndOfLine,
        //   "The period must start on january the first"
        QuotedString("The period must start on january the first"), EndOfLine,
        //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
        FunctionToken(
          ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
          BraceExpressionToken(Seq(Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
            Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1")))
        ), EndOfLine
        // }
      )), EndOfLine,
      // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
      TypeKeyword, Word("CivilYear"), AssignSymbol, Word("Period"), VerifyingKeyword, Word("YearPeriod"),
      VerifyingKeyword, Word("StartJanuaryFirst"), EndOfLine
    )
    val result = SyntaxEnhancer.buildFunctions(input)

    result should ===(expected)
  }
}
