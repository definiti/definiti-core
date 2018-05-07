package definiti.common.plugin

import definiti.common.ast.{Library, Root}
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation.Validated

trait ValidatorPlugin extends Plugin {
  def validate(root: Root, library: Library): Validated[NoResult]
}