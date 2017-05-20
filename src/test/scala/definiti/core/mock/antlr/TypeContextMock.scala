package definiti.core.mock.antlr

import java.util.{List => JList}

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class AliasTypeContextMock(
  typeNameToken: Token,
  genericTypesContext: Option[GenericTypeListContext],
  referenceTypeNameToken: Token,
  aliasGenericTypesContext: Option[GenericTypeListContext],
  identifiers: Seq[TerminalNode],
  docComment: Option[TerminalNode],
  verifyingListContext: Option[VerifyingListContext]
) extends AliasTypeContext(null, 0) {
  this.typeName = typeNameToken
  this.genericTypes = genericTypesContext.orNull
  this.referenceTypeName = referenceTypeNameToken
  this.aliasGenericTypes = aliasGenericTypesContext.orNull

  override def IDENTIFIER(): JList[TerminalNode] = javaList(identifiers)

  override def IDENTIFIER(i: Int): TerminalNode = identifiers(i)

  override def DOC_COMMENT(): TerminalNode = docComment.orNull

  override def verifyingList(): VerifyingListContext = verifyingListContext.orNull

  override def genericTypeList(): JList[GenericTypeListContext] = javaList(Seq(genericTypes, aliasGenericTypes))

  override def genericTypeList(i: Int): GenericTypeListContext = genericTypeList().get(i)
}

object AliasTypeContextMock {
  def apply(aliasTypeContext: AliasTypeContext): AliasTypeContextMock = {
    new AliasTypeContextMock(
      typeNameToken = aliasTypeContext.typeName,
      genericTypesContext = Option(aliasTypeContext.genericTypes),
      referenceTypeNameToken = aliasTypeContext.referenceTypeName,
      aliasGenericTypesContext = Option(aliasTypeContext.aliasGenericTypes),
      identifiers = scalaSeq(aliasTypeContext.IDENTIFIER()),
      docComment = Option(aliasTypeContext.DOC_COMMENT()),
      verifyingListContext = Option(aliasTypeContext.verifyingList())
    )
  }
}

case class DefinedTypeContextMock(
  typeNameToken: Token,
  identifier: TerminalNode,
  docComment: Option[TerminalNode],
  verifyingListContext: Option[VerifyingListContext],
  genericTypeListContext: Option[GenericTypeListContext],
  attributeDefinitionContexts: Seq[AttributeDefinitionContext],
  typeVerificationContexts: Seq[TypeVerificationContext]
) extends DefinedTypeContext(null, 0) {
  this.typeName = typeNameToken

  override def IDENTIFIER(): TerminalNode = identifier

  override def DOC_COMMENT(): TerminalNode = docComment.orNull

  override def verifyingList(): VerifyingListContext = verifyingListContext.orNull

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext.orNull

  override def attributeDefinition(): JList[AttributeDefinitionContext] = javaList(attributeDefinitionContexts)

  override def attributeDefinition(i: Int): AttributeDefinitionContext = attributeDefinitionContexts(i)

  override def typeVerification(): JList[TypeVerificationContext] = javaList(typeVerificationContexts)

  override def typeVerification(i: Int): TypeVerificationContext = typeVerificationContexts(i)
}

object DefinedTypeContextMock {
  def apply(definedTypeContext: DefinedTypeContext): DefinedTypeContextMock = {
    new DefinedTypeContextMock(
      typeNameToken = definedTypeContext.typeName,
      identifier = definedTypeContext.IDENTIFIER(),
      docComment = Option(definedTypeContext.DOC_COMMENT()),
      verifyingListContext = Option(definedTypeContext.verifyingList()),
      genericTypeListContext = Option(definedTypeContext.genericTypeList()),
      attributeDefinitionContexts = scalaSeq(definedTypeContext.attributeDefinition()),
      typeVerificationContexts = scalaSeq(definedTypeContext.typeVerification())
    )
  }
}

case class AttributeDefinitionContextMock(
  attributeNameToken: Token,
  attributeTypeToken: Token,
  verifyingListContext: Option[VerifyingListContext],
  identifiers: Seq[TerminalNode],
  docComment: Option[TerminalNode],
  genericTypeListContext: GenericTypeListContext
) extends AttributeDefinitionContext(null, 0) {
  this.attributeName = attributeNameToken
  this.attributeType = attributeTypeToken

  override def verifyingList(): VerifyingListContext = verifyingListContext.orNull

  override def IDENTIFIER(): JList[TerminalNode] = javaList(identifiers)

  override def IDENTIFIER(i: Int): TerminalNode = identifiers(i)

  override def DOC_COMMENT(): TerminalNode = docComment.orNull

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext
}

object AttributeDefinitionContextMock {
  def apply(attributeDefinitionContext: AttributeDefinitionContext): AttributeDefinitionContextMock = {
    new AttributeDefinitionContextMock(
      attributeNameToken = attributeDefinitionContext.attributeName,
      attributeTypeToken = attributeDefinitionContext.attributeType,
      verifyingListContext = Option(attributeDefinitionContext.verifyingList()),
      identifiers = scalaSeq(attributeDefinitionContext.IDENTIFIER()),
      docComment = Option(attributeDefinitionContext.DOC_COMMENT()),
      genericTypeListContext = attributeDefinitionContext.genericTypeList()
    )
  }
}

case class TypeVerificationContextMock(
  verificationMessageToken: Token,
  typeVerificationFunctionContext: TypeVerificationFunctionContext,
  string: TerminalNode
) extends TypeVerificationContext(null, 0) {
  this.verificationMessage = verificationMessageToken

  override def typeVerificationFunction(): TypeVerificationFunctionContext = typeVerificationFunctionContext

  override def STRING(): TerminalNode = string
}

case class TypeVerificationFunctionContextMock(
  identifier: TerminalNode,
  chainedExpressionContext: ChainedExpressionContext
) extends TypeVerificationFunctionContext(null, 0) {
  override def IDENTIFIER(): TerminalNode = identifier

  override def chainedExpression(): ChainedExpressionContext = chainedExpressionContext
}