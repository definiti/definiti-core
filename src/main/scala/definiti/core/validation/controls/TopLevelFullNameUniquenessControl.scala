package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert

private[core] object TopLevelFullNameUniquenessControl extends Control {
  override val description: String = "Check if there is two top-level statements with the same full name (package + name)"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    val elements = extractElements(library)
    controlUniqueness(elements)
  }

  private def extractElements(library: Library): Seq[ElementInformation] = {
    Seq(
      library.verifications.map(verification => ElementInformation(verification.fullName, verification.location)),
      library.projectTypes.map(projectType => ElementInformation(projectType.fullName, projectType.location)),
      library.namedFunctions.map(namedFunction => ElementInformation(namedFunction.fullName, namedFunction.location))
    ).flatten
  }

  private def controlUniqueness(elements: Seq[ElementInformation]): ControlResult = {
    ControlResult.squash {
      elements.zipWithIndex.map { case (element, index) =>
        val indexOfElement = elements.indexWhere(_.fullName == element.fullName)
        if (indexOfElement != index) {
          val otherElement = elements(indexOfElement)
          ControlResult(errorSameName(element.fullName, element.location, otherElement.location))
        } else {
          OK
        }
      }
    }
  }

  def errorSameName(conflictName: String, location1: Location, location2: Location): Seq[Alert] = {
    Seq(location1, location2).map { location =>
      alert(s"The name ${conflictName} is used at least twice in this project", location)
    }
  }

  case class ElementInformation(fullName: String, location: Location)

}
