package definiti.core

import definiti.core.ProgramResult.NoResult
import definiti.core.validation.Controls
import definiti.core.validation.controls.{ControlLevel, ControlResult}

sealed trait Program[A] {
  def map[B](op: A => B): Program[B] = MapProgram(this, op)

  def flatMap[B](op: A => Program[B]): Program[B] = FlatMapProgram(this, op)

  def withFilter(predicate: A => Boolean): Program[A] = WithFilterProgram(this, predicate)

  def run(configuration: Configuration): ProgramResult[A]
}

object Program {
  def apply[A](value: A): Program[A] = new PureProgram[A](value)

  // TODO: Should remove validated and validation to keep only one thing
  def validated[A](validated: Validated[A]): Program[A] = new ValidatedProgram[A](validated)

  def validation(validation: Validation): Program[NoResult] = ValidationProgram(validation)

  def apply(result: ControlResult): Program[NoResult] = ControlResultProgram(result)

  def apply[A](result: ProgramResult[A]): Program[A] = ResultProgram(result)

  private[core] def runWithControl[A](program: Program[A], configuration: Configuration): ProgramResult[A] = {
    control(program.run(configuration), configuration)
  }

  private[core] def control[A](programResult: ProgramResult[A], configuration: Configuration): ProgramResult[A] = {
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

  private[core] def hasBlockingAlert(alerts: Seq[Alert], configuration: Configuration): Boolean = {
    alerts.exists {
      case AlertControl(control, _, _) =>
        val controlLevel = configuration.userFlags.get(control)
          .orElse(Controls.all.find(_.name == control).map(_.defaultLevel))
          .getOrElse(ControlLevel.ignored)
        controlLevel >= configuration.fatalLevel
      case _ => false
    }
  }
}

private[core] case class PureProgram[A](value: A) extends Program[A] {
  override def run(configuration: Configuration): ProgramResult[A] = Ok(value)
}

private[core] case class MapProgram[A, B](program: Program[A], op: A => B) extends Program[B] {
  override def run(configuration: Configuration): ProgramResult[B] = Program.runWithControl(program, configuration) match {
    case Ok(value, alerts) => Ok(op(value), alerts)
    case Ko(alerts) => Ko(alerts)
  }
}

private[core] case class FlatMapProgram[A, B](program: Program[A], op: A => Program[B]) extends Program[B] {
  override def run(configuration: Configuration): ProgramResult[B] = Program.runWithControl(program, configuration) match {
    case Ok(value, alerts) => Program.runWithControl(op(value), configuration) match {
      case Ok(innerValue, innerAlerts) => Ok(innerValue, alerts ++ innerAlerts)
      case Ko(innerAlerts) => Ko(alerts ++ innerAlerts)
    }
    case Ko(alerts) => Ko(alerts)
  }
}

private[core] case class WithFilterProgram[A](program: Program[A], predicate: A => Boolean) extends Program[A] {
  override def run(configuration: Configuration): ProgramResult[A] = Program.runWithControl(program, configuration) match {
    case Ok(value, alerts) =>
      if (predicate(value)) {
        Ok(value, alerts)
      } else {
        Ko(alerts)
      }
    case Ko(alerts) => Ko(alerts)
  }
}

private[core] case class ValidatedProgram[A](validated: Validated[A]) extends Program[A] {
  override def run(configuration: Configuration): ProgramResult[A] = validated match {
    case Invalid(errors) => Ko(errors.map(Alert(_)))
    case ValidValue(value) => Ok(value)
    case Valid => Ok(NoResult).asInstanceOf[ProgramResult[A]]
  }
}

private[core] case class ValidationProgram(validation: Validation) extends Program[NoResult] {
  override def run(configuration: Configuration): ProgramResult[NoResult] = validation match {
    case Invalid(errors) => Ko(errors.map(Alert(_)))
    case Valid => Ok(NoResult)
  }
}

private[core] case class ControlResultProgram(result: ControlResult) extends Program[NoResult] {
  override def run(configuration: Configuration): ProgramResult[NoResult] = {
    if (Program.hasBlockingAlert(result.alerts, configuration)) {
      Ko(result.alerts)
    } else {
      Ok(NoResult, result.alerts)
    }
  }
}

private[core] case class ResultProgram[A](result: ProgramResult[A]) extends Program[A] {
  override def run(configuration: Configuration): ProgramResult[A] = Program.control(result, configuration)
}