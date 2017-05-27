package definiti.core.validation

import definiti.core.Range

private[core] sealed trait Validation {
  def join(other: Validation): Validation

  def verifyingAlso(nextValidation: => Validation): Validation
}

private[core] object Validation {
  def join(validations: Seq[Validation]): Validation = {
    validations.foldLeft(Valid.asInstanceOf[Validation]) { (acc, validation) => acc.join(validation) }
  }

  def join(validations: Validation*)(implicit dummyImplicit: DummyImplicit): Validation = {
    join(validations)
  }
}

private[core] case object Valid extends Validation {
  override def join(other: Validation): Validation = other

  override def verifyingAlso(nextValidation: => Validation): Validation = nextValidation
}

private[core] case class Invalid(errors: Seq[Error]) extends Validation {
  def join(other: Validation): Validation = other match {
    case Valid => this
    case Invalid(otherErrors) => Invalid(errors ++ otherErrors)
  }

  override def verifyingAlso(nextValidation: => Validation): Validation = this
}

private[core] object Invalid {
  def apply(message: String, range: Range): Invalid = new Invalid(Seq(Error(message, range)))
}

private[core] case class Error(message: String, range: Range) {
  def prettyPrint: String = {
    s"""Error at ${range.prettyPrint}: $message"""
  }
}