package definiti.parser

import definiti._
import definiti.parser.antlr.CoreDefinitionParser._
import definiti.utils.CollectionUtils._
import definiti.utils.ParserUtils._

import scala.collection.mutable.ListBuffer

object CoreDefinitionASTParser {
  def definitionContextToAST(context: CoreDefinitionContext): Seq[ClassDefinition] = {
    val classDefinitions = ListBuffer[ClassDefinition]()

    scalaSeq(context.toplevel()).foreach { element =>
      if (element.coreType() != null) {
        classDefinitions.append(processCoreType(element.coreType()))
      } else {
        // Because all types were treated before, this exception should throw on new element addition.
        throw new RuntimeException("Unexpected token: " + element)
      }
    }

    classDefinitions
  }

  private def processCoreType(context: CoreTypeContext): ClassDefinition = {
    val members = scalaSeq(context.member())
    NativeClassDefinition(
      name = context.typeName.getText,
      attributes = members.filter(_.attribute() != null).map(_.attribute()).map(processAttribute),
      methods = members.filter(_.method() != null).map(_.method()).map(processMethod),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processAttribute(context: AttributeContext): AttributeDefinition = {
    AttributeDefinition(
      name = context.attributeName.getText,
      typeReference = context.attributeType.getText,
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      range = getRangeFromContext(context)
    )
  }

  private def processMethod(context: MethodContext): NativeMethodDefinition = {
    NativeMethodDefinition(
      name = context.methodName.getText,
      parameters = Option(context.parameterListDefinition())
        .map(parameters => scalaSeq(parameters.parameterDefinition()).map(processParameter))
        .getOrElse(Seq.empty),
      returnTypeReference = context.methodType.getText,
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = context.parameterType.getText,
      range = getRangeFromContext(context)
    )
  }
}
