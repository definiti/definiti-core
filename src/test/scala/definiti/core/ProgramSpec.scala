package definiti.core

import definiti.common.ast.Location
import definiti.common.control.{ControlLevel, ControlResult}
import definiti.common.program.ProgramResult.NoResult
import definiti.common.program.{Ko, Ok, Program}
import definiti.common.tests.ConfigurationMock
import definiti.common.validation.{AlertControl, AlertLocation, Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}

class ProgramSpec extends FlatSpec with Matchers {
  import ProgramSpec._

  "Program" should "execute the action" in {
    val program = for {
      start <- Program(1)
      end <- Program(start + 2)
    } yield end
    program.run(configuration.programConfiguration) should ===(Ok(3))
  }

  it should "be executed when run" in {
    var x = 0
    val program = for {
      start <- Program(1)
      middle <- Program {
        x = start
        x + 1
      }
      end <- Program(middle + 2)
    } yield end
    program.run(configuration.programConfiguration)
    x should ===(1)
  }

  it should "not be executed when not run" in {
    var x = 0
    val program = for {
      start <- Program(1)
      middle <- Program {
        x = start
        x + 1
      }
      end <- Program(middle + 2)
    } yield end
    x should ===(0)
    program.run(configuration.programConfiguration)
  }

  it should "pass through valid validations" in {
    val program = for {
      start <- Program(1)
      validation <- Program.validated(Valid(start + 2))
    } yield validation
    program.run(configuration.programConfiguration) should ===(Ok(3))
  }

  it should "be blocked by invalid validations" in {
    val program = for {
      _ <- Program(1)
      validation <- Program.validated[NoResult](Invalid("This is an error", anyLocation))
    } yield validation
    program.run(configuration.programConfiguration) should ===(Ko(Seq(AlertLocation("This is an error", anyLocation))))
  }

  it should "pass through valid result" in {
    val program = for {
      value <- Program(1)
      _ <- Program(ControlResult(Seq.empty))
    } yield value
    program.run(configuration.programConfiguration) should ===(Ok(1))
  }

  it should "be blocked by invalid result" in {
    val program = for {
      value <- Program(1)
      _ <- Program(ControlResult(Seq(alertError)))
    } yield value
    program.run(configuration.programConfiguration) should ===(Ko(Seq(alertError)))
  }

  it should "accept alerts when success" in {
    val program = for {
      start <- Program(1)
      end <- Program(Ok(start + 2, Seq(alertInfo1)))
    } yield end
    program.run(configuration.programConfiguration) should ===(Ok(3, Seq(alertInfo1)))
  }

  it should "accumulate alerts when success" in {
    val program = for {
      start <- Program(1)
      middle <- Program(Ok(start + 2, Seq(alertInfo1)))
      end <- Program(Ok(middle + 3, Seq(alertInfo2)))
    } yield end
    program.run(configuration.programConfiguration) should ===(Ok(6, Seq(alertInfo1, alertInfo2)))
  }

  it should "accumulate alerts and stop on error" in {
    val program = for {
      start <- Program(1)
      _ <- Program(Ok(start + 2, Seq(alertInfo1)))
      middle <- Program(Ko[Int](Seq(alertInfo2)))
      end <- Program(Ok(middle + 3, Seq(alertInfo3)))
    } yield end
    program.run(configuration.programConfiguration) should ===(Ko(Seq(alertInfo1, alertInfo2)))
  }

  it should "be invalid in terms of the configuration - stop on error" in {
    val program = for {
      start <- Program(1)
      middle1 <- Program(Ok(start + 2, Seq(alertInfo1)))
      middle2 <- Program(Ok(middle1 + 3, Seq(alertError)))
      end <- Program(Ok(middle2 + 4, Seq(alertInfo3)))
    } yield end
    program.run(configuration.programConfiguration) should ===(Ko(Seq(alertInfo1, alertError)))
  }

  it should "be invalid in terms of the configuration - stop on info" in {
    val program = for {
      start <- Program(1)
      middle1 <- Program(Ok(start + 2, Seq(alertInfo1)))
      middle2 <- Program(Ok(middle1 + 3, Seq(alertError)))
      end <- Program(Ok(middle2 + 4, Seq(alertInfo3)))
    } yield end
    val conf = configuration.copy(
      fatalLevel = ControlLevel.info
    )
    program.run(conf.programConfiguration) should ===(Ko(Seq(alertInfo1)))
  }
}

object ProgramSpec {
  val anyLocation = Location("", 0, 0, 0, 0)

  val configuration = ConfigurationMock(
    userFlags = Map(
      "error" -> ControlLevel.error,
      "warning" -> ControlLevel.warning,
      "info" -> ControlLevel.info,
      "ignored" -> ControlLevel.ignored
    )
  )

  val alertInfo1 = AlertControl("info", "an alert", anyLocation)
  val alertInfo2 = AlertControl("info", "a second alert", anyLocation)
  val alertInfo3 = AlertControl("info", "a third alert", anyLocation)

  val alertWarning = AlertControl("warning", "a warning alert", anyLocation)

  val alertError = AlertControl("error", "an error alert", anyLocation)
}