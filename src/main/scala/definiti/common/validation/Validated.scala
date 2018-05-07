package definiti.common.validation

import definiti.common.ast.Location

sealed trait Validated[+A] {
  def isValid: Boolean

  def map[B](f: A => B): Validated[B]

  def flatMap[B](f: A => Validated[B]): Validated[B]

  def and[B](f: => Validated[B]): Validated[B]

  def filter[B](f: A => Validated[Nothing]): Validated[A]

  def foreach(f: A => Unit): Validated[A]

  def fold[B](onError: Seq[Error] => B, onValid: A => B): B

  def prettyPrint: String
}

object Validated {
  def squash[A](validatedSeq: Validated[A]*)(implicit dummyImplicit: DummyImplicit): Validated[Seq[A]] = {
    squash(validatedSeq)
  }

  def squash[A](validatedSeq: Seq[Validated[A]]): Validated[Seq[A]] = {
    if (validatedSeq.forall(_.isValid)) {
      Valid(validatedSeq.collect { case Valid(values) => values })
    } else {
      Invalid(validatedSeq.collect { case Invalid(errors) => errors }.flatten)
    }
  }

  def flatSquash[A](validatedSeq: Validated[Seq[A]]*)(implicit dummyImplicit: DummyImplicit): Validated[Seq[A]] = {
    flatSquash(validatedSeq)
  }

  def flatSquash[A](validatedSeq: Seq[Validated[Seq[A]]]): Validated[Seq[A]] = {
    squash(validatedSeq).map(_.flatten)
  }

  def both[A, B](validatedA: Validated[A], validatedB: Validated[B]): Validated[(A, B)] = {
    (validatedA, validatedB) match {
      case (Invalid(errorsA), Invalid(errorsB)) => Invalid(errorsA ++ errorsB)
      case (Invalid(errorsA), _) => Invalid(errorsA)
      case (_, Invalid(errorsB)) => Invalid(errorsB)
      case (Valid(valueA), Valid(valueB)) => Valid((valueA, valueB))
      case _ => throw new UnsupportedOperationException("Validated.both with Valid")
    }
  }

  def both[A, B, C](validatedA: Validated[A], validatedB: Validated[B], validatedC: Validated[C]): Validated[(A, B, C)] = {
    both(both(validatedA, validatedB), validatedC) match {
      case Invalid(errors) => Invalid(errors)
      case Valid(((valueA, valueB), valueC)) => Valid((valueA, valueB, valueC))
    }
  }

  def reverseOption[A](option: Option[Validated[A]]): Validated[Option[A]] = {
    option match {
      case Some(Valid(value)) => Valid(Some(value))
      case Some(Invalid(errors)) => Invalid(errors)
      case None => Valid(None)
    }
  }
}

case class Valid[+A](value: A) extends Validated[A] {
  override def isValid: Boolean = true

  override def map[B](f: (A) => B): Validated[B] = Valid(f(value))

  override def flatMap[B](f: (A) => Validated[B]): Validated[B] = f(value)

  override def and[B](f: => Validated[B]): Validated[B] = f

  override def filter[B](f: (A) => Validated[Nothing]): Validated[A] = {
    f(value) match {
      case Invalid(errors) => Invalid(errors)
      case _ => this
    }
  }

  override def foreach(f: (A) => Unit): Validated[A] = {
    f(value)
    this
  }

  override def fold[B](onError: (Seq[Error]) => B, onValid: (A) => B): B = onValid(value)

  override def prettyPrint: String = toString
}

case class Invalid(errors: Seq[Error]) extends Validated[Nothing] {
  override def isValid: Boolean = false

  override def map[B](f: (Nothing) => B): Validated[B] = Invalid(errors)

  override def flatMap[B](f: (Nothing) => Validated[B]): Validated[B] = Invalid(errors)

  override def and[B](f: => Validated[B]): Validated[B] = Invalid(errors)

  override def filter[B](f: (Nothing) => Validated[Nothing]): Validated[Nothing] = Invalid(errors)

  override def foreach(f: (Nothing) => Unit): Validated[Nothing] = this

  override def fold[B](onError: (Seq[Error]) => B, onValid: (Nothing) => B): B = onError(errors)

  override def prettyPrint: String = s"Invalid(${errors.map(_.prettyPrint).mkString("\n  ", "\n  ", "\n")})"
}

object Invalid {
  def apply(message: String, location: Location): Invalid = new Invalid(Seq(ASTError(message, location)))
}

sealed trait Error {
  def prettyPrint: String
}

case class ASTError(message: String, location: Location) extends Error {
  def prettyPrint: String = {
    s"""Error at ${location.prettyPrint}: $message"""
  }
}

case class SimpleError(message: String) extends Error {
  override def prettyPrint: String = message
}