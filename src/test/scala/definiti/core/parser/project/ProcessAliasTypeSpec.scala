package definiti.core.parser.project

import definiti.core.generators.Generators
import definiti.core.generators.antlr.TypeContextGenerator
import definiti.core.mock.antlr._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProcessAliasTypeSpec extends FlatSpec with Matchers with PropertyChecks {
  "DefinitiASTParser.processAliasType" should "returns an alias type with defined list of verifications" in {
    val cases = for {
      aliasTypeContext <- TypeContextGenerator.anyAliasTypeContext
      numberOfVerifications <- Gen.posNum[Int]
      verificationNames <- Gen.listOfN(numberOfVerifications, Generators.anyIdentifier)
      verificationMessages <- Gen.listOfN(numberOfVerifications, Gen.option(Generators.anyString))
    } yield (aliasTypeContext, verificationNames, verificationMessages)

    forAll(cases) { case (aliasTypeContext, verificationNames, verificationMessages) =>
      val normalized = AliasTypeContextMock(aliasTypeContext).copy(
        verifyingListContext = Some(VerifyingListContextMock(
          verifyingContexts = verificationNames.zip(verificationMessages).map { case (name, message) =>
            VerifyingContextMock(TokenMock(name), message.map(TokenMock(_)))
          }
        ))
      )
      val result = DefinitiASTParser.processAliasType(normalized)
      result.inherited should not be empty
      result.inherited.map(_.verificationName) should contain allElementsOf verificationNames
      result.inherited.map(_.message) should contain allElementsOf verificationMessages
    }
  }

  it should "returns an alias type with list of verifications if verifyingList is set" in {
    forAll(TypeContextGenerator.aliasTypeContextWithVerifyingList) { attributeDefinitionContext =>
      val result = DefinitiASTParser.processAliasType(attributeDefinitionContext)
      result.inherited should not be empty
    }
  }
}
