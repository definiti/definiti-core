package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert

private[core] object VerificationNameUniquenessControl extends Control {
  override val description: String = "Check that each verification name exists once in project, across all packages"
  override val defaultLevel: ControlLevel.Value = ControlLevel.info

  override def control(root: Root, library: Library): ControlResult = {
    controlVerifications(library.verifications)
  }

  private def controlVerifications(verifications: Seq[Verification]): ControlResult = {
    ControlResult.squash {
      verifications.zipWithIndex.map { case (verification, index) =>
        controlVerification(verification, index, verifications)
      }
    }
  }

  private def controlVerification(verification: Verification, index: Int, verifications: Seq[Verification]): ControlResult = {
    val indexOfVerification = verifications.indexWhere(_.name == verification.name)
    if (indexOfVerification != index) {
      duplicateName(verifications(indexOfVerification).fullName, verification.fullName, verification.location)
    } else {
      OK
    }
  }

  def duplicateName(first: String, second: String, location: Location): Alert = {
    alert(s"Verifications ${first} and ${second} have the same name (package removed)", location)
  }
}
