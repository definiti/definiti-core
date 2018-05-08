package definiti.common.program

import definiti.common.control.{ControlLevel, ControlResult}
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation._

sealed trait Program[A] {
  def map[B](op: A => B): Program[B] = MapProgram(this, op)

  def flatMap[B](op: A => Program[B]): Program[B] = FlatMapProgram(this, op)

  def withFilter(predicate: A => Boolean): Program[A] = WithFilterProgram(this, predicate)

  def run(configuration: ProgramConfiguration): ProgramResult[A]
}

object Program {
  def apply[A](value: A): Program[A] = new PureProgram[A](value)

  // TODO: Should remove validated to keep only one thing
  def validated[A](validated: Validated[A]): Program[A] = new ValidatedProgram[A](validated)

  def apply(result: ControlResult): Program[NoResult] = ControlResultProgram(result)

  def apply[A](result: ProgramResult[A]): Program[A] = ResultProgram(result)

  private[program] def runWithControl[A](program: Program[A], configuration: ProgramConfiguration): ProgramResult[A] = {
    control(program.run(configuration), configuration)
  }

  private[program] def control[A](programResult: ProgramResult[A], configuration: ProgramConfiguration): ProgramResult[A] = {
    programResult match {
      case Ok(value, alerts) =>
        if (hasBlockingAlert(alerts, configuration)) {
          Ko(alerts)
        } else {
          Ok(value, alerts)
        }
      case Ko(alerts) => Ko(alerts)
    }
  }

  private[program] def hasBlockingAlert(alerts: Seq[Alert], configuration: ProgramConfiguration): Boolean = {
    alerts.exists {
      case AlertControl(control, _, _) =>
        val controlLevel = configuration.userFlags.get(control)
          .orElse(configuration.defaultLevels.get(control))
          .getOrElse(ControlLevel.ignored)
        controlLevel >= configuration.fatalLevel
      case _ => false
    }
  }
}

private[program] case class PureProgram[A](value: A) extends Program[A] {
  override def run(configuration: ProgramConfiguration): ProgramResult[A] = Ok(value)
}

private[program] case class MapProgram[A, B](program: Program[A], op: A => B) extends Program[B] {
  override def run(configuration: ProgramConfiguration): ProgramResult[B] = Program.runWithControl(program, configuration) match {
    case Ok(value, alerts) => Ok(op(value), alerts)
    case Ko(alerts) => Ko(alerts)
  }
}

private[program] case class FlatMapProgram[A, B](program: Program[A], op: A => Program[B]) extends Program[B] {
  override def run(configuration: ProgramConfiguration): ProgramResult[B] = Program.runWithControl(program, configuration) match {
    case Ok(value, alerts) => Program.runWithControl(op(value), configuration) match {
      case Ok(innerValue, innerAlerts) => Ok(innerValue, alerts ++ innerAlerts)
      case Ko(innerAlerts) => Ko(alerts ++ innerAlerts)
    }
    case Ko(alerts) => Ko(alerts)
  }
}

private[program] case class WithFilterProgram[A](program: Program[A], predicate: A => Boolean) extends Program[A] {
  override def run(configuration: ProgramConfiguration): ProgramResult[A] = Program.runWithControl(program, configuration) match {
    case Ok(value, alerts) =>
      if (predicate(value)) {
        Ok(value, alerts)
      } else {
        Ko(alerts)
      }
    case Ko(alerts) => Ko(alerts)
  }
}

private[program] case class ValidatedProgram[A](validated: Validated[A]) extends Program[A] {
  override def run(configuration: ProgramConfiguration): ProgramResult[A] = validated match {
    case Invalid(errors) => Ko(errors.map(Alert(_)))
    case Valid(value) => Ok(value)
  }
}

private[program] case class ControlResultProgram(result: ControlResult) extends Program[NoResult] {
  override def run(configuration: ProgramConfiguration): ProgramResult[NoResult] = {
    if (Program.hasBlockingAlert(result.alerts, configuration)) {
      Ko(result.alerts)
    } else {
      Ok(NoResult, result.alerts)
    }
  }
}

private[program] case class ResultProgram[A](result: ProgramResult[A]) extends Program[A] {
  override def run(configuration: ProgramConfiguration): ProgramResult[A] = Program.control(result, configuration)
}