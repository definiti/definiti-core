package state

import org.scalatest.{FlatSpec, Matchers}


class SyntaxEnhancerScenarioFullSpec extends FlatSpec with Matchers {
  val source = Seq(
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

  val enclosingDone = Seq(
    EndOfLine,
    // verification NonEmpty {
    VerificationKeyword, Word("NonEmpty"), BraceExpressionToken(Seq(EndOfLine,
      //   "The string is empty // quoted comment"
      QuotedString("The string is empty // quoted comment"), EndOfLine,
      //   (string: String) => { string.nonEmpty() }
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol,
      BraceExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq()))), EndOfLine
      // }
    )), EndOfLine,
    // verification NonBlank {
    VerificationKeyword, Word("NonBlank"), BraceExpressionToken(Seq(EndOfLine,
      //   "The string is blank /* quoted comment */"
      QuotedString("The string is blank /* quoted comment */"), EndOfLine,
      //   (string: String) => { string.trim().nonEmpty() }
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol,
      BraceExpressionToken(Seq(
        Word("string"), Dot, Word("trim"), ParenthesisExpressionToken(Seq()),
        Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())
      )), EndOfLine
      // }
    )), EndOfLine,
    // /* \n Could be simplified, but it is for the "example" \n*/
    BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"), EndOfLine,
    // verification PhoneNumber {
    VerificationKeyword, Word("PhoneNumber"), BraceExpressionToken(Seq(EndOfLine,
      //   "Please provide a phone number"
      QuotedString("Please provide a phone number"), EndOfLine,
      //   (string: String) => {
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol, BraceExpressionToken(Seq(EndOfLine,
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
      )), EndOfLine
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
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol,
        BraceExpressionToken(Seq(Word("end"), UpperSymbol, Word("start"), OrSymbol,
          Word("end"), EqualSymbol, Word("start"))), EndOfLine
        //   }
      )), EndOfLine
      // }
    )), EndOfLine,
    // verification YearPeriod {
    VerificationKeyword, Word("YearPeriod"), BraceExpressionToken(Seq(EndOfLine,
      //   "The period must last one year"
      QuotedString("The period must last one year"), EndOfLine,
      //   (period: Period) => {
      ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol, BraceExpressionToken(Seq(EndOfLine,
        //     // Not quite "right" but show the idea
        LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
        //     // hypothese: timestamp in seconds
        LineComment(" hypothese: timestamp in seconds"), EndOfLine,
        //     period.end.timestamp - period.start.timestamp == 365*24*3600
        Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
        Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
        Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600"), EndOfLine
        //   }
      )), EndOfLine
      // }
    )), EndOfLine,
    // verification StartJanuaryFirst {
    VerificationKeyword, Word("StartJanuaryFirst"), BraceExpressionToken(Seq(EndOfLine,
      //   "The period must start on january the first"
      QuotedString("The period must start on january the first"), EndOfLine,
      //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
      ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol,
      BraceExpressionToken(Seq(Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
        Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1"))), EndOfLine
      // }
    )), EndOfLine,
    // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
    TypeKeyword, Word("CivilYear"), AssignSymbol, Word("Period"), VerifyingKeyword, Word("YearPeriod"),
    VerifyingKeyword, Word("StartJanuaryFirst"), EndOfLine
  )

  val ignoreEolDone = Seq(
    // verification NonEmpty {
    VerificationKeyword, Word("NonEmpty"), BraceExpressionToken(Seq(
      //   "The string is empty // quoted comment"
      QuotedString("The string is empty // quoted comment"), EndOfLine,
      //   (string: String) => { string.nonEmpty() }
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol,
      BraceExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())))
      // }
    )), EndOfLine,
    // verification NonBlank {
    VerificationKeyword, Word("NonBlank"), BraceExpressionToken(Seq(
      //   "The string is blank /* quoted comment */"
      QuotedString("The string is blank /* quoted comment */"), EndOfLine,
      //   (string: String) => { string.trim().nonEmpty() }
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol,
      BraceExpressionToken(Seq(
        Word("string"), Dot, Word("trim"), ParenthesisExpressionToken(Seq()),
        Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())
      ))
      // }
    )), EndOfLine,
    // /* \n Could be simplified, but it is for the "example" \n*/
    BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"), EndOfLine,
    // verification PhoneNumber {
    VerificationKeyword, Word("PhoneNumber"), BraceExpressionToken(Seq(
      //   "Please provide a phone number"
      QuotedString("Please provide a phone number"), EndOfLine,
      //   (string: String) => {
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol, BraceExpressionToken(Seq(
        //     if (string.nonEmpty()) {
        IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq()))), BraceExpressionToken(Seq(
          //       if (string.startsWith("+33")) {
          IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("startsWith"), ParenthesisExpressionToken(Seq(QuotedString("+33"))))), BraceExpressionToken(Seq(
            //         string.matches("^\+33\d{9}$")
            Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^\\+33\\d{9}$")))
            //       } else {
          )), ElseKeyword, BraceExpressionToken(Seq(
            //         string.matches("^0\d{9}$")
            Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^0\\d{9}$")))
            //       }
          ))
          //     } else {
        )), ElseKeyword, BraceExpressionToken(Seq(
          //       false
          FalseKeyword
          //     }
        ))
        //   }
      ))
      // }
    )), EndOfLine,
    // type Period {
    TypeKeyword, Word("Period"), BraceExpressionToken(Seq(
      //   start: Date
      Word("start"), Colon, Word("Date"), EndOfLine,
      //   end: Date
      Word("end"), Colon, Word("Date"), EndOfLine,
      //   verify {
      VerifyKeyword, BraceExpressionToken(Seq(
        //     "end should be after start"
        QuotedString("end should be after start"), EndOfLine,
        //     (period: Period) => { end > start || end == start }
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol,
        BraceExpressionToken(Seq(Word("end"), UpperSymbol, Word("start"), OrSymbol,
          Word("end"), EqualSymbol, Word("start")))
        //   }
      ))
      // }
    )), EndOfLine,
    // verification YearPeriod {
    VerificationKeyword, Word("YearPeriod"), BraceExpressionToken(Seq(
      //   "The period must last one year"
      QuotedString("The period must last one year"), EndOfLine,
      //   (period: Period) => {
      ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol, BraceExpressionToken(Seq(
        //     // Not quite "right" but show the idea
        LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
        //     // hypothese: timestamp in seconds
        LineComment(" hypothese: timestamp in seconds"), EndOfLine,
        //     period.end.timestamp - period.start.timestamp == 365*24*3600
        Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
        Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
        Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600")
        //   }
      ))
      // }
    )), EndOfLine,
    // verification StartJanuaryFirst {
    VerificationKeyword, Word("StartJanuaryFirst"), BraceExpressionToken(Seq(
      //   "The period must start on january the first"
      QuotedString("The period must start on january the first"), EndOfLine,
      //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
      ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol,
      BraceExpressionToken(Seq(Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
        Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1")))
      // }
    )), EndOfLine,
    // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
    TypeKeyword, Word("CivilYear"), AssignSymbol, Word("Period"), VerifyingKeyword, Word("YearPeriod"),
    VerifyingKeyword, Word("StartJanuaryFirst")
  )

  val firstClassCitizenDone = Seq(
    // verification NonEmpty {
    VerificationToken("NonEmpty", BraceExpressionToken(Seq(
      //   "The string is empty // quoted comment"
      QuotedString("The string is empty // quoted comment"), EndOfLine,
      //   (string: String) => { string.nonEmpty() }
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol,
      BraceExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())))
      // }
    ))),
    // verification NonBlank {
    VerificationToken("NonBlank", BraceExpressionToken(Seq(
      //   "The string is blank /* quoted comment */"
      QuotedString("The string is blank /* quoted comment */"), EndOfLine,
      //   (string: String) => { string.trim().nonEmpty() }
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol,
      BraceExpressionToken(Seq(
        Word("string"), Dot, Word("trim"), ParenthesisExpressionToken(Seq()),
        Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())
      ))
      // }
    ))),
    // /* \n Could be simplified, but it is for the "example" \n*/
    BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"),
    // verification PhoneNumber {
    VerificationToken("PhoneNumber", BraceExpressionToken(Seq(
      //   "Please provide a phone number"
      QuotedString("Please provide a phone number"), EndOfLine,
      //   (string: String) => {
      ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))), MapSymbol, BraceExpressionToken(Seq(
        //     if (string.nonEmpty()) {
        IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq()))), BraceExpressionToken(Seq(
          //       if (string.startsWith("+33")) {
          IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("startsWith"), ParenthesisExpressionToken(Seq(QuotedString("+33"))))), BraceExpressionToken(Seq(
            //         string.matches("^\+33\d{9}$")
            Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^\\+33\\d{9}$")))
            //       } else {
          )), ElseKeyword, BraceExpressionToken(Seq(
            //         string.matches("^0\d{9}$")
            Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^0\\d{9}$")))
            //       }
          ))
          //     } else {
        )), ElseKeyword, BraceExpressionToken(Seq(
          //       false
          FalseKeyword
          //     }
        ))
        //   }
      ))
      // }
    ))),
    // type Period {
    TypeToken("Period", Right(BraceExpressionToken(Seq(
      //   start: Date
      Word("start"), Colon, Word("Date"), EndOfLine,
      //   end: Date
      Word("end"), Colon, Word("Date"), EndOfLine,
      //   verify {
      VerifyKeyword, BraceExpressionToken(Seq(
        //     "end should be after start"
        QuotedString("end should be after start"), EndOfLine,
        //     (period: Period) => { end > start || end == start }
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol,
        BraceExpressionToken(Seq(Word("end"), UpperSymbol, Word("start"), OrSymbol,
          Word("end"), EqualSymbol, Word("start")))
        //   }
      ))
      // }
    ))), Seq()),
    // verification YearPeriod {
    VerificationToken("YearPeriod", BraceExpressionToken(Seq(
      //   "The period must last one year"
      QuotedString("The period must last one year"), EndOfLine,
      //   (period: Period) => {
      ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol, BraceExpressionToken(Seq(
        //     // Not quite "right" but show the idea
        LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
        //     // hypothese: timestamp in seconds
        LineComment(" hypothese: timestamp in seconds"), EndOfLine,
        //     period.end.timestamp - period.start.timestamp == 365*24*3600
        Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
        Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
        Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600")
        //   }
      ))
      // }
    ))),
    // verification StartJanuaryFirst {
    VerificationToken("StartJanuaryFirst", BraceExpressionToken(Seq(
      //   "The period must start on january the first"
      QuotedString("The period must start on january the first"), EndOfLine,
      //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
      ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))), MapSymbol,
      BraceExpressionToken(Seq(Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
        Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1")))
      // }
    ))),
    // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
    TypeToken("CivilYear", Left("Period"), Seq("YearPeriod", "StartJanuaryFirst"))
  )

  val functionsDone = Seq(
    // verification NonEmpty {
    VerificationToken("NonEmpty", BraceExpressionToken(Seq(
      //   "The string is empty // quoted comment"
      QuotedString("The string is empty // quoted comment"), EndOfLine,
      //   (string: String) => { string.nonEmpty() }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())))
      )
      // }
    ))),
    // verification NonBlank {
    VerificationToken("NonBlank", BraceExpressionToken(Seq(
      //   "The string is blank /* quoted comment */"
      QuotedString("The string is blank /* quoted comment */"), EndOfLine,
      //   (string: String) => { string.trim().nonEmpty() }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(
          Word("string"), Dot, Word("trim"), ParenthesisExpressionToken(Seq()),
          Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())
        ))
      )
      // }
    ))),
    // /* \n Could be simplified, but it is for the "example" \n*/
    BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"),
    // verification PhoneNumber {
    VerificationToken("PhoneNumber", BraceExpressionToken(Seq(
      //   "Please provide a phone number"
      QuotedString("Please provide a phone number"), EndOfLine,
      //   (string: String) => {
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(
          //     if (string.nonEmpty()) {
          IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq()))), BraceExpressionToken(Seq(
            //       if (string.startsWith("+33")) {
            IfKeyword, ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("startsWith"), ParenthesisExpressionToken(Seq(QuotedString("+33"))))), BraceExpressionToken(Seq(
              //         string.matches("^\+33\d{9}$")
              Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^\\+33\\d{9}$")))
              //       } else {
            )), ElseKeyword, BraceExpressionToken(Seq(
              //         string.matches("^0\d{9}$")
              Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^0\\d{9}$")))
              //       }
            ))
            //     } else {
          )), ElseKeyword, BraceExpressionToken(Seq(
            //       false
            FalseKeyword
            //     }
          ))
          //   }
        ))
      )
      // }
    ))),
    // type Period {
    TypeToken("Period", Right(BraceExpressionToken(Seq(
      //   start: Date
      Word("start"), Colon, Word("Date"), EndOfLine,
      //   end: Date
      Word("end"), Colon, Word("Date"), EndOfLine,
      //   verify {
      VerifyKeyword, BraceExpressionToken(Seq(
        //     "end should be after start"
        QuotedString("end should be after start"), EndOfLine,
        FunctionToken(
          //     (period: Period) => { end > start || end == start }
          ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
          BraceExpressionToken(Seq(Word("end"), UpperSymbol, Word("start"), OrSymbol,
            Word("end"), EqualSymbol, Word("start")))
        )
        //   }
      ))
      // }
    ))), Seq()),
    // verification YearPeriod {
    VerificationToken("YearPeriod", BraceExpressionToken(Seq(
      //   "The period must last one year"
      QuotedString("The period must last one year"), EndOfLine,
      //   (period: Period) => {
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
        BraceExpressionToken(Seq(
          //     // Not quite "right" but show the idea
          LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
          //     // hypothese: timestamp in seconds
          LineComment(" hypothese: timestamp in seconds"), EndOfLine,
          //     period.end.timestamp - period.start.timestamp == 365*24*3600
          Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
          Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
          Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600")
          //   }
        ))
      )
      // }
    ))),
    // verification StartJanuaryFirst {
    VerificationToken("StartJanuaryFirst", BraceExpressionToken(Seq(
      //   "The period must start on january the first"
      QuotedString("The period must start on january the first"), EndOfLine,
      //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
        BraceExpressionToken(Seq(Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
          Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1")))
      )
      // }
    ))),
    // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
    TypeToken("CivilYear", Left("Period"), Seq("YearPeriod", "StartJanuaryFirst"))
  )

  val conditionDone = Seq(
    // verification NonEmpty {
    VerificationToken("NonEmpty", BraceExpressionToken(Seq(
      //   "The string is empty // quoted comment"
      QuotedString("The string is empty // quoted comment"), EndOfLine,
      //   (string: String) => { string.nonEmpty() }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())))
      )
      // }
    ))),
    // verification NonBlank {
    VerificationToken("NonBlank", BraceExpressionToken(Seq(
      //   "The string is blank /* quoted comment */"
      QuotedString("The string is blank /* quoted comment */"), EndOfLine,
      //   (string: String) => { string.trim().nonEmpty() }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(
          Word("string"), Dot, Word("trim"), ParenthesisExpressionToken(Seq()),
          Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())
        ))
      )
      // }
    ))),
    // /* \n Could be simplified, but it is for the "example" \n*/
    BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"),
    // verification PhoneNumber {
    VerificationToken("PhoneNumber", BraceExpressionToken(Seq(
      //   "Please provide a phone number"
      QuotedString("Please provide a phone number"), EndOfLine,
      //   (string: String) => {
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(
          //     if (string.nonEmpty()) {
          ConditionToken(
            ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq()))),
            BraceExpressionToken(Seq(
              //       if (string.startsWith("+33")) {
              ConditionToken(
                ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("startsWith"), ParenthesisExpressionToken(Seq(QuotedString("+33"))))),
                BraceExpressionToken(Seq(
                  //         string.matches("^\+33\d{9}$")
                  Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^\\+33\\d{9}$")))
                  //       } else {
                )),
                Some(BraceExpressionToken(Seq(
                  //         string.matches("^0\d{9}$")
                  Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^0\\d{9}$")))
                  //       }
                )))
              )
              //     } else {
            )),
            Some(BraceExpressionToken(Seq(
              //       false
              FalseKeyword
              //     }
            )))
          )
          //   }
        ))
      )
      // }
    ))),
    // type Period {
    TypeToken("Period", Right(BraceExpressionToken(Seq(
      //   start: Date
      Word("start"), Colon, Word("Date"), EndOfLine,
      //   end: Date
      Word("end"), Colon, Word("Date"), EndOfLine,
      //   verify {
      VerifyKeyword, BraceExpressionToken(Seq(
        //     "end should be after start"
        QuotedString("end should be after start"), EndOfLine,
        FunctionToken(
          //     (period: Period) => { end > start || end == start }
          ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
          BraceExpressionToken(Seq(Word("end"), UpperSymbol, Word("start"), OrSymbol,
            Word("end"), EqualSymbol, Word("start")))
        )
        //   }
      ))
      // }
    ))), Seq()),
    // verification YearPeriod {
    VerificationToken("YearPeriod", BraceExpressionToken(Seq(
      //   "The period must last one year"
      QuotedString("The period must last one year"), EndOfLine,
      //   (period: Period) => {
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
        BraceExpressionToken(Seq(
          //     // Not quite "right" but show the idea
          LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
          //     // hypothese: timestamp in seconds
          LineComment(" hypothese: timestamp in seconds"), EndOfLine,
          //     period.end.timestamp - period.start.timestamp == 365*24*3600
          Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
          Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
          Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600")
          //   }
        ))
      )
      // }
    ))),
    // verification StartJanuaryFirst {
    VerificationToken("StartJanuaryFirst", BraceExpressionToken(Seq(
      //   "The period must start on january the first"
      QuotedString("The period must start on january the first"), EndOfLine,
      //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
        BraceExpressionToken(Seq(Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
          Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1")))
      )
      // }
    ))),
    // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
    TypeToken("CivilYear", Left("Period"), Seq("YearPeriod", "StartJanuaryFirst"))
  )

  val firstClassCitizenCompletedDone = Seq(
    // verification NonEmpty {
    StructuredVerificationToken(
      "NonEmpty",
      //   "The string is empty // quoted comment"
      "The string is empty // quoted comment",
      //   (string: String) => { string.nonEmpty() }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())))
      )
      // }
    ),
    // verification NonBlank {
    StructuredVerificationToken(
      "NonBlank",
      //   "The string is blank /* quoted comment */"
      "The string is blank /* quoted comment */",
      //   (string: String) => { string.trim().nonEmpty() }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(
          Word("string"), Dot, Word("trim"), ParenthesisExpressionToken(Seq()),
          Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq())
        ))
      )
      // }
    ),
    // /* \n Could be simplified, but it is for the "example" \n*/
    BlockComment("\n" + "  Could be simplified, but it is for the \"example\"\n"),
    // verification PhoneNumber {
    StructuredVerificationToken(
      "PhoneNumber",
      //   "Please provide a phone number"
      "Please provide a phone number",
      //   (string: String) => {
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("string"), Colon, Word("String"))),
        BraceExpressionToken(Seq(
          //     if (string.nonEmpty()) {
          ConditionToken(
            ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("nonEmpty"), ParenthesisExpressionToken(Seq()))),
            BraceExpressionToken(Seq(
              //       if (string.startsWith("+33")) {
              ConditionToken(
                ParenthesisExpressionToken(Seq(Word("string"), Dot, Word("startsWith"), ParenthesisExpressionToken(Seq(QuotedString("+33"))))),
                BraceExpressionToken(Seq(
                  //         string.matches("^\+33\d{9}$")
                  Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^\\+33\\d{9}$")))
                  //       } else {
                )),
                Some(BraceExpressionToken(Seq(
                  //         string.matches("^0\d{9}$")
                  Word("string"), Dot, Word("matches"), ParenthesisExpressionToken(Seq(QuotedString("^0\\d{9}$")))
                  //       }
                )))
              )
              //     } else {
            )),
            Some(BraceExpressionToken(Seq(
              //       false
              FalseKeyword
              //     }
            )))
          )
          //   }
        ))
      )
      // }
    ),
    // type Period {
    StructuredDefinedTypeToken(
      "Period",
      Seq(
        //   start: Date
        TypeFieldToken("start", "Date"),
        //   end: Date
        TypeFieldToken("end", "Date")
      ),
      //   verify {
      Seq(
        TypeVerificationToken(
          //     "end should be after start"
          "end should be after start",
          FunctionToken(
            //     (period: Period) => { end > start || end == start }
            ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
            BraceExpressionToken(Seq(Word("end"), UpperSymbol, Word("start"), OrSymbol,
              Word("end"), EqualSymbol, Word("start")))
          )
          //   }
        )
      ),
      Seq()
      // }
    ),
    // verification YearPeriod {
    StructuredVerificationToken(
      "YearPeriod",
      //   "The period must last one year"
      "The period must last one year",
      //   (period: Period) => {
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
        BraceExpressionToken(Seq(
          //     // Not quite "right" but show the idea
          LineComment(" Not quite \"right\" but show the idea"), EndOfLine,
          //     // hypothese: timestamp in seconds
          LineComment(" hypothese: timestamp in seconds"), EndOfLine,
          //     period.end.timestamp - period.start.timestamp == 365*24*3600
          Word("period"), Dot, Word("end"), Dot, Word("timestamp"), MinusSymbol,
          Word("period"), Dot, Word("start"), Dot, Word("timestamp"), EqualSymbol,
          Word("365"), TimeSymbol, Word("24"), TimeSymbol, Word("3600")
          //   }
        ))
      )
      // }
    ),
    // verification StartJanuaryFirst {
    StructuredVerificationToken(
      "StartJanuaryFirst",
      //   "The period must start on january the first"
      "The period must start on january the first",
      //   (period: Period) => { period.start.day == 1 && period.start.month == 1 }
      FunctionToken(
        ParenthesisExpressionToken(Seq(Word("period"), Colon, Word("Period"))),
        BraceExpressionToken(Seq(Word("period"), Dot, Word("start"), Dot, Word("day"), EqualSymbol, Word("1"), AndSymbol,
          Word("period"), Dot, Word("start"), Dot, Word("month"), EqualSymbol, Word("1")))
      )
      // }
    ),
    // type CivilYear = Period verifying YearPeriod verifying StartJanuaryFirst
    StructuredAliasTypeToken("CivilYear", "Period", Seq("YearPeriod", "StartJanuaryFirst"))
  )

  "SyntaxEnhancer.buildEnclosing" should "create Enclosing expression recursively" in {
    SyntaxEnhancer.buildEnclosing(source) should ===(enclosingDone)
  }

  "SyntaxEnhancer.ignoreEOLInEncloser" should "remove all ignored EndOfLine inside encloser" in {
    SyntaxEnhancer.ignoreEOLInEncloser(enclosingDone) should ===(ignoreEolDone)
  }

  "SyntaxEnhancer.buildFirstClassCitizen" should "build first class citizen" in {
    SyntaxEnhancer.buildFirstClassCitizen(ignoreEolDone) should ===(firstClassCitizenDone)
  }

  "SyntaxEnhancer.buildFunctions" should "create functions through the tree" in {
    SyntaxEnhancer.buildFunctions(firstClassCitizenDone) should ===(functionsDone)
  }

  "SyntaxEnhancer.buildConditions" should "create conditions through the tree" in {
    SyntaxEnhancer.buildConditions(functionsDone) should ===(conditionDone)
  }

  "SyntaxEnhancer.completeFirstClassCitizenStructure" should "complete first class citizen with a more accurate definition" in {
    SyntaxEnhancer.completeFirstClassCitizenStructure(conditionDone) should ===(firstClassCitizenCompletedDone)
  }
}
