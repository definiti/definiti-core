package definiti.core.mock.antlr

import java.util.{List => JList}

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import org.antlr.v4.runtime.tree.TerminalNode

case class DefinitiContextMock(
  packageNameContext: Option[PackageNameContext],
  importsContextSeq: Seq[ImportsContext],
  toplevelContextSeq: Seq[ToplevelContext]
) extends DefinitiContext(null, 0) {
  override def packageName: PackageNameContext = packageNameContext.orNull

  override def imports: JList[ImportsContext] = javaList(importsContextSeq)

  override def imports(i: Int): ImportsContext = importsContextSeq(i)

  override def toplevel: JList[ToplevelContext] = javaList(toplevelContextSeq)

  override def toplevel(i: Int): ToplevelContext = toplevelContextSeq(i)
}

case class PackageNameContextMock(
  dottedIdentifierContext: DottedIdentifierContext
) extends PackageNameContext(null, 0) {
  override def dottedIdentifier: DottedIdentifierContext = dottedIdentifierContext
}

case class ImportsContextMock(
  dottedIdentifierContext: DottedIdentifierContext
) extends ImportsContext(null, 0) {
  override def dottedIdentifier: DottedIdentifierContext = dottedIdentifierContext
}

case class DottedIdentifierContextMock(
  identifiers: Seq[TerminalNode]
) extends DottedIdentifierContext(null, 0) {
  override def IDENTIFIER: JList[TerminalNode] = javaList(identifiers)

  override def IDENTIFIER(i: Int): TerminalNode = identifiers(i)
}

abstract class ToplevelContextMock extends ToplevelContext(null, 0) {
  override def verification(): VerificationContext = null

  override def definedType(): DefinedTypeContext = null

  override def aliasType(): AliasTypeContext = null
}

case class ToplevelVerificationContextMock(
  verificationContext: VerificationContext
) extends ToplevelContextMock {
  override def verification(): VerificationContext = verificationContext
}

case class ToplevelDefinedTypeContextMock(
  definedTypeContext: DefinedTypeContext
) extends ToplevelContextMock {
  override def definedType(): DefinedTypeContext = definedTypeContext
}

case class ToplevelAliasTypeContextMock(
  aliasTypeContext: AliasTypeContext
) extends ToplevelContextMock {
  override def aliasType(): AliasTypeContext = aliasTypeContext
}
