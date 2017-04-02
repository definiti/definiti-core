package state

object Sample {
  // TODO: change verification to assertion, predicate or better keyword
  val testString: String =
    """
      |verification NonEmpty {
      |  "The string is empty // quoted comment"
      |  (string: String) => { string.nonEmpty() }
      |}
      |
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
      |    (period: Period) => { period.end > period.start || period.end == period.start }
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
}
