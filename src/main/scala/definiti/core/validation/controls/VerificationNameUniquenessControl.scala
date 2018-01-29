package definiti.core.validation.controls

import definiti.core.ast._

object VerificationNameUniquenessControl extends Control {
  override val name: String = "verificationNameUniqueness"
  override val description: String = "Check that each verification name exists once in project, across all packages"
  override val defaultLevel: ControlLevel.Value = ControlLevel.info

  override def control(root: Root, library: Library): ControlResult = {
    controlVerifications(library.verifications)
  }

  private def controlVerifications(verifications: Seq[Verification]): ControlResult = {
    ControlResult.squash {
      verifications.zipWithIndex.map { case (verification, index) =>
        val indexOfVerification = verifications.indexWhere(_.name == verification.name)
        if (indexOfVerification != index) {
          val otherVerification = verifications(indexOfVerification)
          alert(s"Verifications ${otherVerification.fullName} and ${verification.fullName} have the same name (package removed)", verification.location)
        } else {
          OK
        }
      }
    }
  }
}
