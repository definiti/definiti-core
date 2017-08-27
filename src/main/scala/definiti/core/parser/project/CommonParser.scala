package definiti.core.parser.project

import definiti.core.{ParameterDefinition, TypeReference}
import definiti.core.parser.antlr.DefinitiParser.{GenericTypeListContext, ParameterDefinitionContext, ParameterListDefinitionContext}
import definiti.core.utils.CollectionUtils.scalaSeq
import definiti.core.utils.ParserUtils.getRangeFromContext

trait CommonParser {
  def processParameterListDefinition(context: ParameterListDefinitionContext): Seq[ParameterDefinition] = {
    scalaSeq(context.parameterDefinition()).map(processParameter)
  }

  def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = TypeReference(context.parameterType.getText, processGenericTypeList(context.genericTypeList())),
      getRangeFromContext(context)
    )
  }

  def processGenericTypeList(context: GenericTypeListContext): Seq[TypeReference] = {
    if (context != null) {
      scalaSeq(context.genericType()).map { genericTypeContext =>
        TypeReference(
          genericTypeContext.IDENTIFIER().getText,
          processGenericTypeList(genericTypeContext.genericTypeList())
        )
      }
    } else {
      Seq()
    }
  }
}
