package definiti.core.validation

import definiti.common.ast._
import definiti.common.control.{Control, ControlResult}
import definiti.common.program.ProgramResult.NoResult
import definiti.common.program.{Program, ProgramConfiguration}
import definiti.core.validation.controls._

class Controls(configuration: ProgramConfiguration) {
  def validate(root: Root, library: Library): Program[NoResult] = Program {
    ControlResult.squash {
      Controls.all
        .filter(configuration.isControlAccepted)
        .map(_.control(root, library))
    }
  }
}

object Controls {
  lazy val all: Seq[Control[Root]] = Seq(
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
    TypeParameterUsableControl,
    TypeDeclarationParametersControl,
    TypeVerificationIsBooleanControl,
    TypeVerificationIsOkKoControl,
    TypeVerificationParameterUsableControl,
    VerificationIsBooleanControl,
    VerificationIsOkKoControl,
    VerificationNameUniquenessControl,
    VerificationReferenceControl,
    VerificationReferenceParametersControl,
    VerificationTypeControl
  )
}