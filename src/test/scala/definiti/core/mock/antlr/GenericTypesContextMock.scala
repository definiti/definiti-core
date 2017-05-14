package definiti.core.mock.antlr

import java.util.{List => JList}

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils.javaList
import org.antlr.v4.runtime.tree.TerminalNode

case class GenericTypeListContextMock(
  genericTypeContexts: Seq[GenericTypeContext]
) extends GenericTypeListContext(null, 0) {
  override def genericType(): JList[GenericTypeContext] = javaList(genericTypeContexts)

  override def genericType(i: Int): GenericTypeContext = genericTypeContexts(i)
}

case class GenericTypeContextMock(
  identifier: TerminalNode,
  genericTypeListContext: Option[GenericTypeListContext]
) extends GenericTypeContext(null, 0) {
  override def IDENTIFIER(): TerminalNode = identifier

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext.orNull
}