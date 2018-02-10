package definiti.core.validation

import definiti.core.ProgramResult.NoResult
import definiti.core._
import definiti.core.ast.{Library, Root}
import definiti.core.validation.controls._

class Controls(configuration: Configuration) {
  def validate(root: Root, library: Library): Program[NoResult] = Program {
    ControlResult.squash {
      Controls.all
        .filter(isControlAccepted)
        .map(_.control(root, library))
    }
  }

  private def isControlAccepted(control: Control): Boolean = {
    configuration.controlLevels(control.name) >= configuration.controlLevel
  }
}

object Controls {
  lazy val all: Seq[Control] = Seq(
    AliasTypeTypeControl,
    AttributeTypeControl,
    CalculatorOperandsAreNumberControl,
    EnumerationUniquenessControl,
    EqualityOnSameTypeControl,
    FunctionParametersControl,
    LogicalOperandsAreBooleanControl,
    MethodParametersControl,
    NamedFunctionTypeControl,
    NotExpressionIsBooleanControl,
    OrderOperandsAreNumberControl,
    VerificationParameterUsableControl,
    TopLevelFullNameUniquenessControl,
    TypeNameFormatControl,
    TypeVerificationIsBooleanControl,
    TypeVerificationIsOkKoControl,
    VerificationIsBooleanControl,
    VerificationIsOkKoControl,
    VerificationNameUniquenessControl,
    VerificationReferenceControl,
    VerificationReferenceParametersControl,
    VerificationTypeControl
  )
}