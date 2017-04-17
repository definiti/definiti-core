package definiti.api

import definiti.{ClassDefinition, Verification}

sealed trait Context {
  def isTypeAvailable(typeName: String): Boolean

  def findType(typeName: String): Option[ClassDefinition]

  def isVerificationAvailable(verificationName: String): Boolean

  def findVerification(verificationName: String): Option[Verification]
}

case class ReferenceContext(
  classes: Seq[ClassDefinition],
  verifications: Seq[Verification]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    classes.exists(_.name == typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
    classes.find(_.name == typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    verifications.exists(_.name == verificationName)
  }

  override def findVerification(verificationName: String): Option[Verification] = {
    verifications.find(_.name == verificationName)
  }
}

case class FunctionContext(
  referenceContext: ReferenceContext,
  currentType: ClassDefinition
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentType.name == typeName ||
      // TODO: currentType.genericTypes.contains(typeName) ||
      referenceContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
    if (currentType.name == typeName) {
      Some(currentType)
    } else {
      referenceContext.findType(typeName)
    }
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    referenceContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[Verification] = {
    referenceContext.findVerification(verificationName)
  }
}