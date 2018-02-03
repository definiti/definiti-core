package definiti.core.validation

import definiti.core._
import definiti.core.ast._

private[core] trait TypeValidation {
  self: ASTValidation =>

  protected def validateEnum(enum: Enum): Validation = {
    Validation.join {
      enum.cases.zipWithIndex.map { case (enumCase, index) =>
        if (enum.cases.indexWhere(_.name == enumCase.name) == index) {
          Valid
        } else {
          Invalid(s"The case ${enumCase.name} is already defined in enum ${enum.name}", enumCase.location)
        }
      }
    }
  }
}
