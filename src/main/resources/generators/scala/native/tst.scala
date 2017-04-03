package object booleanWrapperUtils {
  implicit def booleanToBooleanWrapper(boolean: Boolean): BooleanWrapper = new BooleanWrapper(boolean)
  implicit def booleanWrapperToBoolean(booleanWrapper: BooleanWrapper): Boolean = booleanWrapper.inner
}

import booleanWrapperUtils._

class BooleanWrapper(val inner: Boolean) {

}
import java.util.Date

// TODO: Use newer Date API
class DateWrapper(private val inner: Date) {
  def timestamp: NumberWrapper = new NumberWrapper(inner.getTime())

  def day: NumberWrapper = new NumberWrapper(inner.getDate)

  def month: NumberWrapper = new NumberWrapper(inner.getMonth)

  def ==(dateWrapper: DateWrapper): BooleanWrapper = timestamp == dateWrapper.timestamp

  def !=(dateWrapper: DateWrapper): BooleanWrapper = timestamp != dateWrapper.timestamp

  def >(dateWrapper: DateWrapper): BooleanWrapper = timestamp > dateWrapper.timestamp

  def <(dateWrapper: DateWrapper): BooleanWrapper = timestamp < dateWrapper.timestamp

  def >=(dateWrapper: DateWrapper): BooleanWrapper = timestamp >= dateWrapper.timestamp

  def <=(dateWrapper: DateWrapper): BooleanWrapper = timestamp <= dateWrapper.timestamp
}
class NumberWrapper(private val inner: BigDecimal) {
  def -(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner - numberWrapper.inner)

  def +(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner + numberWrapper.inner)

  def *(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner * numberWrapper.inner)

  def /(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner / numberWrapper.inner)

  def %(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner % numberWrapper.inner)

  def ==(numberWrapper: NumberWrapper): BooleanWrapper = inner == numberWrapper.inner

  def !=(numberWrapper: NumberWrapper): BooleanWrapper = inner != numberWrapper.inner

  def >(numberWrapper: NumberWrapper): BooleanWrapper = inner > numberWrapper.inner

  def <(numberWrapper: NumberWrapper): BooleanWrapper = inner < numberWrapper.inner

  def >=(numberWrapper: NumberWrapper): BooleanWrapper = inner >= numberWrapper.inner

  def <=(numberWrapper: NumberWrapper): BooleanWrapper = inner <= numberWrapper.inner
}
class StringWrapper(private val inner: String) {
  def nonEmpty(): BooleanWrapper = inner.nonEmpty

  def trim(): StringWrapper = new StringWrapper(inner.trim)

  def startsWith(prefix: StringWrapper): BooleanWrapper = inner.startsWith(prefix.inner)

  def matches(regex: StringWrapper): BooleanWrapper = inner.matches(regex.inner)
}

package object verifications {


  def verifyNonEmpty(string: StringWrapper): Option[String] = {
    verify("The string is empty // quoted comment") {
      (string).nonEmpty()
    }
  }




  def verifyNonBlank(string: StringWrapper): Option[String] = {
    verify("The string is blank /* quoted comment */") {
      ((string).trim()).nonEmpty()
    }
  }



  /*
    Could be simplified, but it is for the "example"
  */
  def verifyPhoneNumber(string: StringWrapper): Option[String] = {
    verify("Please provide a phone number") {

      if ((string).nonEmpty()) {

        if ((string).startsWith(new StringWrapper("+33"))) {
          (string).matches(new StringWrapper("^\\+33\\d{9}$"))
        } else {
          (string).matches(new StringWrapper("^0\\d{9}$"))
        }

      } else {
        new BooleanWrapper(false)
      }

    }
  }




  def verifyYearPeriod(period: $Period): Option[String] = {
    verify("The period must last one year") {
      ((((period).end).timestamp) - (((period).start).timestamp)) == ((new NumberWrapper(365)) * ((new NumberWrapper(24)) * (new NumberWrapper(3600))))
    }
  }




  def verifyStartJanuaryFirst(period: $Period): Option[String] = {
    verify("The period must start on january the first") {
      ((((period).start).day) == (new NumberWrapper(1))) && ((((period).start).month) == (new NumberWrapper(1)))
    }
  }


  private def verify(message: String)(condition: => Boolean) = {
    if (condition) {
      None
    } else {
      Some(message)
    }
  }
}
import verifications._



trait $Period {
  def start: DateWrapper
  def end: DateWrapper
}


class Period private(val start: DateWrapper, val end: DateWrapper) extends $Period

object Period {
  def apply(start: DateWrapper, end: DateWrapper): Either[String, Period] = {
    val __result = new Period(start, end)
    val period = __result
    val __errorOpt = Seq(

      verify("end should be after start") {
        (((period).end) > ((period).start)) || (((period).end) == ((period).start))
      }

    ).find(_.isDefined).flatten

    __errorOpt match {
      case Some(error) => Left(error)
      case None =>

        Right(__result)
    }
  }

  private def verify(message: String)(condition: => Boolean) = {
    if (condition) {
      None
    } else {
      Some(message)
    }
  }
}





class CivilYear private(val start: DateWrapper, val end: DateWrapper) extends $Period

object CivilYear {
  def apply(start: DateWrapper, end: DateWrapper): Either[String, CivilYear] = {
    val __result = new CivilYear(start, end)
    val period = __result
    val __errorOpt = Seq(

      verify("end should be after start") {
        (((period).end) > ((period).start)) || (((period).end) == ((period).start))
      }
      , verifyYearPeriod(__result), verifyStartJanuaryFirst(__result)
    ).find(_.isDefined).flatten

    __errorOpt match {
      case Some(error) => Left(error)
      case None =>

        Right(new CivilYear(__result.start, __result.end))
    }
  }

  private def verify(message: String)(condition: => Boolean) = {
    if (condition) {
      None
    } else {
      Some(message)
    }
  }
}