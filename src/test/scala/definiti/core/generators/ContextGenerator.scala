package definiti.core.generators

import definiti.core.ReferenceContext
import org.scalacheck.Gen

import scala.annotation.tailrec

object ContextGenerator {
  def anyContext(implicit context: ReferenceContext): Gen[ReferenceContext] = for {
    verifications <- Gen.listOf(VerificationGenerator.anyReferencedValidVerification)
    types <- Gen.listOf(TypeGenerator.anyType)
  } yield {
    val usedNamed = context.verifications.map(_.canonicalName) ++ context.classes.map(_.canonicalName)
    val noDuplicatedVerifications = verifications.filterNot(verification => usedNamed.contains(verification.canonicalName))
    val noDuplicatedTypes = types.filterNot(aType => usedNamed.contains(aType.canonicalName))

    context.copy(
      classes = context.classes ++ noDuplicatedTypes,
      verifications = context.verifications ++ noDuplicatedVerifications
    )
  }

  def anyContextWithNonEmptyVerifications(implicit context: ReferenceContext): Gen[ReferenceContext] = for {
    verifications <- Gen.nonEmptyListOf(VerificationGenerator.anyReferencedValidVerification)
    types <- Gen.listOf(TypeGenerator.anyType)
  } yield {
    val usedNamed = context.verifications.map(_.canonicalName) ++ context.classes.map(_.canonicalName)
    val noDuplicatedVerifications = verifications.map(verification => verification.copy(name = makeUnique(verification.name, context)))
    val noDuplicatedTypes = types.filterNot(aType => usedNamed.contains(aType.canonicalName))

    context.copy(
      classes = context.classes ++ noDuplicatedTypes,
      verifications = context.verifications ++ noDuplicatedVerifications
    )
  }

  @tailrec
  def makeUnique(name: String, context: ReferenceContext): String = {
    def existsClassWithName = context.classes.exists(_.canonicalName == name)

    def existsVerificationWithName = context.verifications.exists(_.canonicalName == name)

    if (existsClassWithName || existsVerificationWithName) {
      val char = Gen.alphaNumChar.sample.getOrElse('a')
      makeUnique(name + char, context)
    } else {
      name
    }
  }
}
