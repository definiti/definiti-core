package definiti.core.parser.project

import definiti.core.{ParameterDefinition, TypeReference}
import definiti.core.parser.antlr.DefinitiParser.{GenericTypeContext, GenericTypeListContext, ParameterDefinitionContext, ParameterListDefinitionContext}
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
      scalaSeq(context.genericType()).map(processGenericType)
    } else {
      Seq()
    }
  }

  def processGenericType(context: GenericTypeContext): TypeReference = {
    TypeReference(
      context.IDENTIFIER().getText,
      processGenericTypeList(context.genericTypeList())
    )
  }
}
