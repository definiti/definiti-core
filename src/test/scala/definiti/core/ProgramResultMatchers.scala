package definiti.core

import org.scalatest.matchers.{MatchResult, Matcher}

trait ProgramResultMatchers {
  import org.scalatest.Matchers._

  class ProgramResultMatcher[A](expected: ProgramResult[A]) extends Matcher[ProgramResult[A]] {

    def apply(left: ProgramResult[A]): MatchResult = {
      (expected, left) match {
        case (Ok(_, _), Ko(_)) => failed(expected, left)
        case (Ko(_), Ok(_, _)) => failed(expected, left)
        case (Ok(expectedValue, _), Ok(gotValue, _)) if expectedValue != gotValue => failed(expected, left)
        case (expectedKo: Ko[_], gotKo: Ko[_]) if expectedKo.prettyPrint != gotKo.prettyPrint => failed(expected, left)
        case _ => success
      }
    }

    private val success = MatchResult(matches = true, "", "")

    private def failed(expected: ProgramResult[A], got: ProgramResult[A]) = MatchResult(matches = false, s"${expected.prettyPrint} did not equal ${got.prettyPrint}", "")
  }

  def beResult[A](expected: ProgramResult[A]) = new ProgramResultMatcher[A](expected)

  def ok[A] = a[Ok[A]]

  def ko[A] = a[Ko[A]]
}

object ProgramResultMatchers extends ProgramResultMatchers
