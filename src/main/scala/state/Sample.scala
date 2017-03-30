package state

object Sample {
  // TODO: change verification to assertion, predicate or better keyword
  val testString: String =
    """
      |verification NonEmpty {
      |  "The string is empty"
      |  (string: String) => { string.nonEmpty() }
      |}
      |
      |verification NonBlank {
      |  "The string is blank"
      |  (string: String) => { string.trim.nonEmpty() }
      |}
      |
      |// Could be simplified, but it is for the example
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
      |    // Not quite right but show the idea
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

  val sampleAST = TopLevel(
    verifications = Seq(
      Verification(
        name = "The string is empty",
        message = "The string is empty",
        check = DefinedFunction(
          name = "",
          parameters = Seq(Parameter("string", "String")),
          expression = CallMethod(VariableExpression("string"), "nonEmpty", Seq())
        )
      ),
      Verification(
        name = "NonBlank",
        message = "The string is blank",
        check = DefinedFunction(
          name = "",
          parameters = Seq(Parameter("string", "String")),
          expression = CallMethod(CallMethod(VariableExpression("string"), "trim", Seq()), "nonEmpty", Seq())
        )
      ),
      Verification(
        name = "PhoneNumber",
        message = "Please provide a phone number",
        check = DefinedFunction(
          name = "",
          parameters = Seq(Parameter("string", "String")),
          expression = If(
            condition = CallMethod(VariableExpression("string"), "nonEmpty", Seq()),
            whenTrue = If(
              condition = CallMethod(VariableExpression("string"), "startsWith", Seq(StringValueExpression("+33"))),
              whenTrue = CallMethod(VariableExpression("string"), "matches", Seq(StringValueExpression("^\\+33\\d{9}$"))),
              whenFalse = CallMethod(VariableExpression("string"), "matches", Seq(StringValueExpression("^0\\d{9}$")))
            ),
            whenFalse = False
          )
        )
      ),
      Verification(
        name = "YearPeriod",
        message = "The period must last one year",
        check = DefinedFunction(
          name = "",
          parameters = Seq(Parameter("period", "Period")),
          expression = Equals(
            Minus(
              CallAttribute(CallAttribute(VariableExpression("period"), "end"), "timestamp"),
              CallAttribute(CallAttribute(VariableExpression("period"), "start"), "timestamp")
            ),
            Time(
              NumberValueExpression(365),
              Time(
                NumberValueExpression(24),
                NumberValueExpression(3600)
              )
            )
          )
        )
      ),
      Verification(
        name = "StartJanuaryFirst",
        message = "The period must start on january the first",
        check = DefinedFunction(
          name = "",
          parameters = Seq(Parameter("period", "Period")),
          expression = And(
            Equals(
              CallAttribute(CallAttribute(VariableExpression("period"), "start"), "day"),
              NumberValueExpression(1)
            ),
            Equals(
              CallAttribute(CallAttribute(VariableExpression("period"), "start"), "month"),
              NumberValueExpression(1)
            )
          )
        )
        //(period: Period) => period.start.day == 1 && period.start.month == 1
      ),
      Verification(
        name = "Period$verify$1",
        message = "end should be after start",
        check = DefinedFunction(
          name = "",
          parameters = Seq(Parameter("period", "Period")),
          expression = Or(
            Upper(
              VariableExpression("end"),
              VariableExpression("start")
            ),
            Equals(
              VariableExpression("end"),
              VariableExpression("start")
            )
          )
        )
      )
    ),
    types = Seq(
      Type(
        name = "Period",
        attributes = Seq(
          Attribute(
            name = "start",
            typeReference = "Date"
          ),
          Attribute(
            name = "end",
            typeReference = "Date"
          )
        ),
        verifications = Seq("Period$verify$1")
      ),
      Type(
        name = "CivilYear",
        attributes = Seq(
          Attribute(
            name = "start",
            typeReference = "Date"
          ),
          Attribute(
            name = "end",
            typeReference = "Date"
          )
        ),
        verifications = Seq("Period$verify$1", "YearPeriod", "StartJanuaryFirst")
      )
    )
  )
}
