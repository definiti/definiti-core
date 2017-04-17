package definiti.api

import definiti.{ClassDefinition, TypeReference, Verification}

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

case class ClassContext(
  outerContext: Context,
  currentType: ClassDefinition,
  genericTypes: Seq[ClassReference]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentType.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
    if (currentType.genericTypes.contains(typeName)) {
      val indexOfGeneric = currentType.genericTypes.indexOf(typeName)
      if (indexOfGeneric < genericTypes.size) {
        outerContext.findType(genericTypes(indexOfGeneric).classDefinition.name)
      } else {
        None
      }
    } else {
      outerContext.findType(typeName)
    }
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[Verification] = {
    outerContext.findVerification(verificationName)
  }
}