package definiti.core.validation

import definiti.core.ProgramResult.NoResult
import definiti.core._
import definiti.core.ast.{Library, Root}
import definiti.core.validation.controls._

class Controls(configuration: Configuration) {
  private lazy val controlLevels: Map[String, ControlLevel.Value] = {
    Controls.all
      .map { control => control.name -> configuration.userFlags.getOrElse(control.name, control.defaultLevel) }
      .toMap
  }

  def validate(root: Root, library: Library): Program[NoResult] = Program {
    ControlResult.squash {
      Controls.all
        .filter(isControlAccepted)
        .map(_.control(root, library))
    }
  }

  private def isControlAccepted(control: Control): Boolean = {
    controlLevels(control.name) >= configuration.controlLevel
  }
}

object Controls {
  lazy val all: Seq[Control] = Seq(
    AliasTypeTypeControl,
    AttributeTypeControl,
    EnumerationUniquenessControl,
    NamedFunctionTypeControl,
    TypeNameFormatControl,
    VerificationIsBooleanControl,
    VerificationNameUniquenessControl,
    VerificationReferenceControl,
    VerificationTypeControl
  )
}