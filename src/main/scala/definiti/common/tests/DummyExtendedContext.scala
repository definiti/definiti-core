package definiti.common.tests

import definiti.common.ast.{Library, Location}
import definiti.common.plugin.ContextPlugin
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation.{Valid, Validated}

class DummyExtendedContext extends ContextPlugin[String] {
  override def contextName = "dummyContext"

  override def parse(content: String, packageName: String, imports: Map[String, String], location: Location): String = content

  override def validate(context: String, library: Library): Validated[NoResult] = Valid(NoResult)

  override def name = "dummyContext"

  override def contextToJson(context: String): String = context

  override def contextFromJson(json: String): String = json
}
