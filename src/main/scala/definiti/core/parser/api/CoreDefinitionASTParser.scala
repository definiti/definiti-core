package definiti.core.parser.api

import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.parser.antlr.CoreDefinitionParser._
import definiti.core.parser.project.CommonParser
import definiti.core.utils.CollectionUtils._

import scala.collection.mutable.ListBuffer

private[core] class CoreDefinitionASTParser(sourceFile: String) extends CommonParser {
  val file: String = sourceFile.replaceAllLiterally("\\", "/")

  def definitionContextToAST(context: CoreDefinitionContext): Seq[PureClassDefinition] = {
    val classDefinitions = ListBuffer[PureClassDefinition]()

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

  private def processCoreType(context: CoreTypeContext): PureClassDefinition = {
    val members = scalaSeq(context.member())
    PureNativeClassDefinition(
      name = context.typeName.getText,
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq()),
      attributes = members.filter(_.attributeDefinition() != null).map(_.attributeDefinition()).map(processAttribute),
      methods = members.filter(_.methodDefinition() != null).map(_.methodDefinition()).map(processMethod),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processAttribute(context: AttributeDefinitionContext): PureAttributeDefinition = {
    PureAttributeDefinition(
      name = context.attributeName.getText,
      typeDeclaration = processTypeReferenceAsDeclaration(context.typeReference),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment),
      verifications = Seq.empty,
      location = getLocationFromContext(context)
    )
  }

  private def processTypeReferenceAsDeclaration(context: TypeReferenceContext): PureTypeDeclaration = {
    PureTypeDeclaration(
      typeName = context.IDENTIFIER.getText,
      genericTypes = Option(context.typeReferenceList).map(processTypeReferenceListAsDeclaration).getOrElse(Seq.empty),
      parameters = Seq.empty,
      location = getLocationFromContext(context)
    )
  }

  private def processTypeReferenceListAsDeclaration(context: TypeReferenceListContext): Seq[PureTypeDeclaration] = {
    scalaSeq(context.typeReference).map(processTypeReferenceAsDeclaration)
  }

  private def processMethod(context: MethodDefinitionContext): MethodDefinition = {
    MethodDefinition(
      name = context.methodName.getText,
      genericTypes = Option(context.genericTypeList())
        .map(genericTypes => scalaSeq(genericTypes.genericType()).map(_.getText))
        .getOrElse(Seq.empty),
      parameters = Option(context.parameterListDefinition())
        .map(parameters => scalaSeq(parameters.parameterDefinition()).map(processParameter))
        .getOrElse(Seq.empty),
      returnType = processTypeReference(context.methodType),
      comment = Option(context.DOC_COMMENT()).map(_.getText).map(extractDocComment)
    )
  }

  private def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = processAbstractTypeReference(context.abstractTypeReference()),
      location = getLocationFromContext(context)
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

  private def processLambdaReference(lambdaReference: => LambdaReferenceContext): LambdaReference = {
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

  private def processTypeReference(context: TypeReferenceContext): TypeReference = {
    TypeReference(
      context.IDENTIFIER().getText,
      Option(context.typeReferenceList).map(processTypeReferenceList).getOrElse(Seq.empty)
    )
  }

  private def processTypeReferenceList(context: TypeReferenceListContext): Seq[TypeReference] = {
    scalaSeq(context.typeReference).map(processTypeReference)
  }
}
