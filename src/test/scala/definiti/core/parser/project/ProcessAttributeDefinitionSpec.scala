package definiti.core.parser.project

import definiti.core.ConfigurationMock
import definiti.core.generators.Generators
import definiti.core.generators.antlr.TypeContextGenerator
import definiti.core.mock.antlr.{AttributeDefinitionContextMock, TokenMock, VerifyingContextMock, VerifyingListContextMock}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProcessAttributeDefinitionSpec extends FlatSpec with Matchers with PropertyChecks {
  private val configuration = ConfigurationMock()
  private val definitiASTParser = new DefinitiASTParser("test.def", configuration)

  "DefinitiASTParser.processAttributeDefinition" should "returns an attribute definition with defined list of verifications" in {
    val cases = for {
      attributeDefinitionContext <- TypeContextGenerator.anyAttributeDefinitionContext
      numberOfVerifications <- Gen.posNum[Int]
      verificationNames <- Gen.listOfN(numberOfVerifications, Generators.anyIdentifier)
      verificationMessages <- Gen.listOfN(numberOfVerifications, Gen.option(Generators.anyString))
    } yield (attributeDefinitionContext, verificationNames, verificationMessages)

    forAll(cases) { case (attributeDefinitionContext, verificationNames, verificationMessages) =>
      val normalized = AttributeDefinitionContextMock(attributeDefinitionContext).copy(
        verifyingListContext = Some(VerifyingListContextMock(
          verifyingContexts = verificationNames.zip(verificationMessages).map { case (name, message) =>
            VerifyingContextMock(TokenMock(name), message.map(TokenMock(_)))
          }
        ))
      )
      val result = definitiASTParser.processAttributeDefinition(normalized)
      result.verifications should not be empty
      result.verifications.map(_.verificationName) should contain allElementsOf verificationNames
      result.verifications.map(_.message) should contain allElementsOf verificationMessages
    }
  }

  it should "returns an attribute definition with list of verifications if verifyingList is set" in {
    forAll(TypeContextGenerator.attributeDefinitionContextWithVerifyingList) { attributeDefinitionContext =>
      val result = definitiASTParser.processAttributeDefinition(attributeDefinitionContext)
      result.verifications should not be empty
    }
  }
}
