package definiti.core.mock.antlr

import definiti.core.parser.antlr.DefinitiParser._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class TypeReferenceContextMock(
  typeNameToken: Token,
  genericTypeListContext: GenericTypeListContext
) extends TypeReferenceContext(null, 0) {
  this.typeName = typeNameToken

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(typeNameToken)

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext
}
