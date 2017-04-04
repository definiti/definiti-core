package definiti.api

import definiti.{ClassDefinition, Verification}

import scala.collection.mutable.ListBuffer

object TypeReference {
  private val typeReferences = ListBuffer[ClassDefinition]()

  private val verifications = ListBuffer[Verification]()

  def referenceType(classDefinition: ClassDefinition): Unit = {
    typeReferences.append(classDefinition)
  }

  def findType(name: String): Option[ClassDefinition] = {
    typeReferences.find(_.name == name)
  }

  def referenceVerification(verification: Verification): Unit = {
    verifications.append(verification)
  }

  def findVerification(name: String): Option[Verification] = {
    verifications.find(_.name == name)
  }
}
