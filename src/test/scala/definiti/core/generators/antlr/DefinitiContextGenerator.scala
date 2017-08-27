package definiti.core.generators.antlr

import definiti.core.mock.antlr._
import definiti.core.parser.antlr.DefinitiParser._
import org.scalacheck.Gen

object DefinitiContextGenerator {
  lazy val anyDefinitiContext: Gen[DefinitiContext] = for {
    packageName <- Gen.option(anyPackageName)
    importsContextSeq <- Gen.listOf(anyImportsContext)
    toplevelContextSeq <- Gen.listOf(anyToplevelContext)
  } yield {
    DefinitiContextMock(
      packageName,
      importsContextSeq,
      toplevelContextSeq
    )
  }

  lazy val anyPackageName: Gen[PackageNameContext] = AntlrGenerator.genContext(for {
    dottedIdentifierContext <- anyDottedIdentifierContext
  } yield {
    PackageNameContextMock(dottedIdentifierContext)
  })

  lazy val anyImportsContext: Gen[ImportsContext] = AntlrGenerator.genContext(for {
    dottedIdentifierContext <- anyDottedIdentifierContext
  } yield {
    ImportsContextMock(dottedIdentifierContext)
  })

  lazy val anyDottedIdentifierContext: Gen[DottedIdentifierContext] = AntlrGenerator.genContext(for {
    identifiers <- Gen.listOf(AntlrGenerator.anyIdentifierNode)
  } yield {
    DottedIdentifierContextMock(identifiers)
  })

  lazy val anyToplevelContext: Gen[ToplevelContext] = Gen.oneOf(
    anyToplevelVerificationContext,
    anyToplevelDefinedTypeContextMock,
    anyToplevelAliasTypeContextMock,
    anyToplevelContextContextMock
  )

  lazy val anyToplevelVerificationContext: Gen[ToplevelVerificationContextMock] = {
    AntlrGenerator.genContext(VerificationContextGenerator.anyVerificationContext.map(ToplevelVerificationContextMock))
  }

  lazy val anyToplevelDefinedTypeContextMock: Gen[ToplevelDefinedTypeContextMock] = {
    AntlrGenerator.genContext(TypeContextGenerator.anyDefinedTypeContext.map(ToplevelDefinedTypeContextMock))
  }

  lazy val anyToplevelAliasTypeContextMock: Gen[ToplevelAliasTypeContextMock] = {
    AntlrGenerator.genContext(TypeContextGenerator.anyAliasTypeContext.map(ToplevelAliasTypeContextMock))
  }

  lazy val anyToplevelContextContextMock: Gen[ToplevelContextContextMock] = {
    AntlrGenerator.genContext(ContextGenerator.anyContextContext.map(ToplevelContextContextMock))
  }
}
