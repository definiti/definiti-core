package definiti.core.generators.antlr

import definiti.core.generators.Generators
import definiti.core.mock.antlr.{GenericTypeContextMock, GenericTypeListContextMock}
import definiti.core.parser.antlr.DefinitiParser._
import org.scalacheck.Gen

object GenericTypesContextGenerators {
  lazy val anyGenericTypeListContext: Gen[GenericTypeListContext] = anyGenericTypeListContext(3)

  lazy val anyGenericTypesContext: Gen[GenericTypeContext] = anyGenericTypesContext(3)

  private def anyGenericTypeListContext(limit: Int): Gen[GenericTypeListContext] = AntlrGenerator.genContext(for {
    genericTypeContexts <- Generators.listOfBoundedSize(0, 3, anyGenericTypesContext(limit - 1))
  } yield {
    GenericTypeListContextMock(genericTypeContexts)
  })

  private def anyGenericTypesContext(limit: Int): Gen[GenericTypeContext] = {
    val genericTypeListContextGenerator = limit match {
      case n if n > 0 => Gen.option(anyGenericTypeListContext(n - 1))
      case _ => Gen.const(None)
    }
    AntlrGenerator.genContext(for {
      identifier <- AntlrGenerator.anyIdentifierNode
      genericTypeListContext <- genericTypeListContextGenerator
    } yield {
      GenericTypeContextMock(identifier, genericTypeListContext)
    })
  }
}
