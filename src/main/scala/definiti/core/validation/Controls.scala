package definiti.core.validation

import definiti.common.ast._
import definiti.common.control.{Control, ControlResult}
import definiti.common.program.ProgramResult.NoResult
import definiti.common.program.{Program, ProgramConfiguration}
import definiti.core.validation.controls._
import definiti.core.validation.controls.aliasType._
import definiti.core.validation.controls.naming._

class Controls(configuration: ProgramConfiguration) {
  def validate(root: Root, library: Library): Program[NoResult] = Program.control {
    ControlResult.squash {
      Controls.all
        .filter(configuration.isControlAccepted)
        .map(_.control(root, library))
    }
  }
}

object Controls {
  lazy val all: Seq[Control[Root]] = Seq(
    root,
    aliasType,
    naming
  ).flatten

  private def root: Seq[Control[Root]] = Seq(
    AliasTypeTypeControl,
    AttributeTypeControl,
    AttributeTypeUniquenessControl,
    CalculatorOperandsAreSameNumericControl,
    EnumerationUniquenessControl,
    ComparisonOnSameTypeControl,
    FunctionParametersControl,
    LogicalOperandsAreBooleanControl,
    MethodParametersControl,
    NamedFunctionParameterTypeControl,
    NamedFunctionTypeControl,
    NotExpressionIsBooleanControl,
    OrderOperandsAreSameNumericControl,
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

  private def aliasType: Seq[Control[Root]] = Seq(
    MethodAcceptationControl
  )

  private def naming: Seq[Control[Root]] = Seq(
    NamedFunctionLowerCamelCaseControl,
    NamedFunctionUpperCamelCaseControl,
    TypeLowerCamelCaseControl,
    TypeUpperCamelCaseControl,
    VerificationLowerCamelCaseControl,
    VerificationUpperCamelCaseControl
  )
}