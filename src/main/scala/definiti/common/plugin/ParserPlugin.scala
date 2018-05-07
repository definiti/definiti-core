package definiti.common.plugin

import definiti.common.validation.Validated
import definiti.core.ast.pure.PureRoot

trait ParserPlugin extends Plugin {
  def transform(root: PureRoot): Validated[PureRoot]
}
