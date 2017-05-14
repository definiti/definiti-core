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
  inheritances: Seq[InheritanceContext]
) extends AliasTypeContext(null, 0) {
  this.typeName = typeNameToken
  this.genericTypes = genericTypesContext.orNull
  this.referenceTypeName = referenceTypeNameToken
  this.aliasGenericTypes = aliasGenericTypesContext.orNull

  override def IDENTIFIER(): JList[TerminalNode] = javaList(identifiers)

  override def IDENTIFIER(i: Int): TerminalNode = identifiers(i)

  override def DOC_COMMENT(): TerminalNode = docComment.orNull

  override def inheritance(): JList[InheritanceContext] = javaList(inheritances)

  override def inheritance(i: Int): InheritanceContext = inheritances(i)

  override def genericTypeList(): JList[GenericTypeListContext] = javaList(Seq(genericTypes, aliasGenericTypes))

  override def genericTypeList(i: Int): GenericTypeListContext = genericTypeList().get(i)
}

case class DefinedTypeContextMock(
  typeNameToken: Token,
  identifier: TerminalNode,
  docComment: Option[TerminalNode],
  inheritances: Seq[InheritanceContext],
  genericTypeListContext: Option[GenericTypeListContext],
  attributeDefinitionContexts: Seq[AttributeDefinitionContext],
  typeVerificationContexts: Seq[TypeVerificationContext]
) extends DefinedTypeContext(null, 0) {
  this.typeName = typeNameToken

  override def IDENTIFIER(): TerminalNode = identifier

  override def DOC_COMMENT(): TerminalNode = docComment.orNull

  override def inheritance(): JList[InheritanceContext] = javaList(inheritances)

  override def inheritance(i: Int): InheritanceContext = inheritances(i)

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext.orNull

  override def attributeDefinition(): JList[AttributeDefinitionContext] = javaList(attributeDefinitionContexts)

  override def attributeDefinition(i: Int): AttributeDefinitionContext = attributeDefinitionContexts(i)

  override def typeVerification(): JList[TypeVerificationContext] = javaList(typeVerificationContexts)

  override def typeVerification(i: Int): TypeVerificationContext = typeVerificationContexts(i)
}

case class InheritanceContextMock(
  verificationNameToken: Token,
  identifier: TerminalNode
) extends InheritanceContext(null, 0) {
  this.verificationName = verificationNameToken

  override def IDENTIFIER(): TerminalNode = identifier
}

case class AttributeDefinitionContextMock(
  attributeNameToken: Token,
  attributeTypeToken: Token,
  attributeVerificationsContext: AttributeVerificationsContext,
  identifiers: Seq[TerminalNode],
  docComment: Option[TerminalNode],
  genericTypeListContext: GenericTypeListContext
) extends AttributeDefinitionContext(null, 0) {
  this.attributeName = attributeNameToken
  this.attributeType = attributeTypeToken

  override def attributeVerifications(): AttributeVerificationsContext = attributeVerificationsContext

  override def IDENTIFIER(): JList[TerminalNode] = javaList(identifiers)

  override def IDENTIFIER(i: Int): TerminalNode = identifiers(i)

  override def DOC_COMMENT(): TerminalNode = docComment.orNull

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext
}

case class AttributeVerificationsContextMock(
  identifiers: Seq[TerminalNode]
) extends AttributeVerificationsContext(null, 0) {
  override def IDENTIFIER(): JList[TerminalNode] = javaList(identifiers)

  override def IDENTIFIER(i: Int): TerminalNode = identifiers(i)
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