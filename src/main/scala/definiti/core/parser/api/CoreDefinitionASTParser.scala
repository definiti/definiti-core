package definiti.core.parser.api

import definiti.core._
import definiti.core.parser.antlr.CoreDefinitionParser._
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.ParserUtils._

import scala.collection.mutable.ListBuffer

private[core] object CoreDefinitionASTParser {
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
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq()),
      attributes = members.filter(_.attributeDefinition() != null).map(_.attributeDefinition()).map(processAttribute),
      methods = members.filter(_.methodDefinition() != null).map(_.methodDefinition()).map(processMethod),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processAttribute(context: AttributeDefinitionContext): AttributeDefinition = {
    AttributeDefinition(
      name = context.attributeName.getText,
      typeReference = TypeReference(context.attributeType.getText, processGenericTypeList(context.genericTypeList())),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      verifications = Seq.empty,
      range = getRangeFromContext(context)
    )
  }

  private def processMethod(context: MethodDefinitionContext): NativeMethodDefinition = {
    NativeMethodDefinition(
      name = context.methodName.getText,
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      parameters = Option(context.parameterListDefinition())
        .map(parameters => scalaSeq(parameters.parameterDefinition()).map(processParameter))
        .getOrElse(Seq.empty),
      returnTypeReference = processTypeReference(context.methodType),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = processAbstractTypeReference(context.abstractTypeReference()),
      range = getRangeFromContext(context)
    )
  }

  private def processAbstractTypeReference(context: AbstractTypeReferenceContext): AbstractTypeReference = {
    lazy val typeReference = context.typeReference()
    lazy val lambdaReference = context.lambdaReference()
    if (typeReference != null) {
      processTypeReference(typeReference)
    } else if (lambdaReference != null) {
      processLambdaReference(lambdaReference)
    } else {
      throw new RuntimeException(s"Parameter type: ${context.getText} was not processed")
    }
  }

  private def processLambdaReference(lambdaReference: => LambdaReferenceContext) = {
    if (lambdaReference.inputList != null) {
      LambdaReference(
        inputTypes = scalaSeq(lambdaReference.inputList.typeReference()).map(processTypeReference),
        outputType = processTypeReference(lambdaReference.output)
      )
    } else {
      LambdaReference(
        inputTypes = Seq(processTypeReference(lambdaReference.input)),
        outputType = processTypeReference(lambdaReference.output)
      )
    }
  }

  private def processTypeReference(finalParameterType: TypeReferenceContext): TypeReference = {
    TypeReference(
      finalParameterType.IDENTIFIER().getText,
      processGenericTypeList(finalParameterType.genericTypeList())
    )
  }

  private def processGenericTypeList(context: GenericTypeListContext): Seq[TypeReference] = {
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