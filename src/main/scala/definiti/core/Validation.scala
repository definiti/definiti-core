package definiti.core

// TODO: Remove Valid (and change dependencies) because it can throw exceptions or correct API

sealed trait Validated[+A] {
  def isValid: Boolean

  def map[B](f: A => B): Validated[B]

  def flatMap[B](f: A => Validated[B]): Validated[B]

  def and[B](f: => Validated[B]): Validated[B]

  def filter[B](f: A => Validated[Nothing]): Validated[A]

  def foreach(f: A => Unit): Validated[A]

  def fold[B](onError: Seq[Error] => B, onValid: A => B): B

  def toValidation: Validation
}

object Validated {
  def squash[A](validatedSeq: Validated[A]*)(implicit dummyImplicit: DummyImplicit): Validated[Seq[A]] = {
    squash(validatedSeq)
  }
  def squash[A](validatedSeq: Seq[Validated[A]]): Validated[Seq[A]] = {
    if (validatedSeq.forall(_.isValid)) {
      ValidValue(validatedSeq.collect { case ValidValue(values) => values })
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
      case (ValidValue(valueA), ValidValue(valueB)) => ValidValue((valueA, valueB))
      case _ => throw new UnsupportedOperationException("Validated.both with Valid")
    }
  }
}

case class ValidValue[+A](value: A) extends Validated[A] {
  override def isValid: Boolean = true

  override def map[B](f: (A) => B): Validated[B] = ValidValue(f(value))

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

  override def toValidation: Validation = Valid
}

sealed trait Validation extends Validated[Nothing] {
  def join(other: Validation): Validation

  def verifyingAlso(nextValidation: => Validation): Validation

  override def toValidation: Validation = this
}

object Validation {
  def join(validations: Seq[Validation]): Validation = {
    validations.foldLeft(Valid.asInstanceOf[Validation]) { (acc, validation) => acc.join(validation) }
  }

  def join(validations: Validation*)(implicit dummyImplicit: DummyImplicit): Validation = {
    join(validations)
  }
}

case object Valid extends Validation {
  override def isValid: Boolean = true

  override def map[B](f: (Nothing) => B): Validated[B] = throw new UnsupportedOperationException("Valid.map")

  override def flatMap[B](f: (Nothing) => Validated[B]): Validated[B] = throw new UnsupportedOperationException("Valid.flatMap")

  override def join(other: Validation): Validation = other

  override def verifyingAlso(nextValidation: => Validation): Validation = nextValidation

  override def and[B](f: => Validated[B]): Validated[B] = f

  override def filter[B](f: (Nothing) => Validated[Nothing]): Validated[Nothing] = throw new UnsupportedOperationException("Valid.filter")

  override def foreach(f: (Nothing) => Unit): Validated[Nothing] = throw new UnsupportedOperationException("Valid.foreach")

  override def fold[B](onError: (Seq[Error]) => B, onValid: (Nothing) => B): B = throw new UnsupportedOperationException("Valid.fold")
}

case class Invalid(errors: Seq[Error]) extends Validation {
  override def isValid: Boolean = false

  override def map[B](f: (Nothing) => B): Validated[B] = Invalid(errors)

  override def flatMap[B](f: (Nothing) => Validated[B]): Validated[B] = Invalid(errors)

  def join(other: Validation): Validation = other match {
    case Valid => this
    case Invalid(otherErrors) => Invalid(errors ++ otherErrors)
  }

  override def verifyingAlso(nextValidation: => Validation): Validation = this

  override def and[B](f: => Validated[B]): Validated[B] = Invalid(errors)

  override def filter[B](f: (Nothing) => Validated[Nothing]): Validated[Nothing] = Invalid(errors)

  override def foreach(f: (Nothing) => Unit): Validated[Nothing] = this

  override def fold[B](onError: (Seq[Error]) => B, onValid: (Nothing) => B): B = onError(errors)
}

object Invalid {
  def apply(message: String, range: Range): Invalid = new Invalid(Seq(ASTError(message, range)))
}

sealed trait Error {
  def prettyPrint: String
}

case class ASTError(message: String, range: Range) extends Error {
  def prettyPrint: String = {
    s"""Error at ${range.prettyPrint}: $message"""
  }
}

case class SimpleError(message: String) extends Error {
  override def prettyPrint: String = message
}
