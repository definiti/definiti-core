package definiti.common.program

import cats._
import definiti.common.control.{ControlLevel, ControlResult}
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation._

import scala.annotation.tailrec
import scala.util.Either

case class Program[A](private val process: ProgramConfiguration => ProgramResult[A]) {
  def run(configuration: ProgramConfiguration): ProgramResult[A] = {
    process(configuration): ProgramResult[A]
  }

  def withFilter(predicate: A => Boolean): Program[A] = Program { configuration =>
    process(configuration) match {
      case Ok(value, alerts) if predicate(value) => Ok(value, alerts)
      case Ok(_, alerts) => Ko(alerts)
      case Ko(alerts) => Ko(alerts)
    }
  }
}

object Program {
  def pure[A](value: A): Program[A] = Program(_ => Ok(value))

  def validated[A](validated: Validated[A]): Program[A] = Program { _ =>
    validated match {
      case Invalid(errors) => Ko(errors.map(Alert(_)))
      case Valid(value) => Ok(value)
    }
  }

  def control[A](result: ControlResult): Program[NoResult] = Program { implicit configuration =>
    normalize(Ok(NoResult, result.alerts))
  }

  implicit val functor: Functor[Program] = new Functor[Program] {
    override def map[A, B](program: Program[A])(op: A => B): Program[B] = Program { implicit configuration =>
      normalize(program.process(configuration)) match {
        case Ok(value, alerts) => Ok(op(value), alerts)
        case Ko(alerts) => Ko(alerts)
      }
    }
  }

  implicit val applicative: Applicative[Program] = new Applicative[Program] {
    override def pure[A](value: A): Program[A] = Program.pure(value)

    override def ap[A, B](programF: Program[A => B])(program: Program[A]): Program[B] = Program { implicit configuration =>
      normalize(program.process(configuration)) match {
        case Ok(value, alerts) =>
          normalize(programF.process(configuration)) match {
            case Ok(aToB, alertsAtoB) => Ok(aToB(value), alerts ++ alertsAtoB)
            case Ko(alertsAtoB) => Ko(alerts ++ alertsAtoB)
          }
        case Ko(alerts) => Ko(alerts)
      }
    }
  }

  implicit val monad: Monad[Program] = new Monad[Program] {
    override def pure[A](value: A): Program[A] = Program.pure(value)

    override def flatMap[A, B](program: Program[A])(nextProgram: A => Program[B]): Program[B] = Program { implicit configuration =>
      normalize(program.process(configuration)) match {
        case Ok(value, alerts) =>
          normalize(nextProgram(value).process(configuration)) match {
            case Ok(nextValue, nextAlerts) => Ok(nextValue, alerts ++ nextAlerts)
            case Ko(nextAlerts) => Ko(alerts ++ nextAlerts)
          }
        case Ko(alerts) => Ko(alerts)
      }
    }

    override def tailRecM[A, B](initialValue: A)(nextProgram: A => Program[Either[A, B]]): Program[B] = Program { implicit configuration =>
      @tailrec
      def process(acc: A, alertsAcc: Seq[Alert]): ProgramResult[B] = {
        normalize(nextProgram(acc).process(configuration)) match {
          case Ok(Left(nextValue), alerts) => process(nextValue, alertsAcc ++ alerts)
          case Ok(Right(finalValue), alerts) => Ok(finalValue, alertsAcc ++ alerts)
          case Ko(alerts) => Ko(alertsAcc ++ alerts)
        }
      }

      process(initialValue, Seq.empty)
    }
  }

  private def normalize[A](programResult: ProgramResult[A])(implicit configuration: ProgramConfiguration): ProgramResult[A] = {
    programResult match {
      case Ok(_, alerts) if hasBlockingAlert(alerts, configuration) => Ko(alerts)
      case ok: Ok[A] => ok
      case ko: Ko[A] => ko
    }
  }

  private def hasBlockingAlert(alerts: Seq[Alert], configuration: ProgramConfiguration): Boolean = {
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