package definiti.common.plugin

import definiti.common.ast.Root
import definiti.common.validation.Validated

trait ParserPlugin extends Plugin {
  def transform(root: Root): Validated[Root]
}
