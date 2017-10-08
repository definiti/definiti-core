package definiti.core.parser.project

import definiti.core.ConfigurationMock
import definiti.core.ast._
import definiti.core.generators.Generators
import definiti.core.generators.antlr.TypeContextGenerator
import definiti.core.mock.antlr._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class ProcessDefinedTypeSpec extends FlatSpec with Matchers with PropertyChecks {
  private val configuration = ConfigurationMock()
  private val definitiASTParser = new DefinitiASTParser("test.def", configuration)

  "DefinitiASTParser.processDefinedType" should "have only TypeVerification with DefinedTypeName" in {
    forAll(TypeContextGenerator.anyDefinedTypeContext) { definedTypeContext =>
      val result = definitiASTParser.processDefinedType(definedTypeContext)

      Inspectors.forAll(result.verifications.toList) { verification =>
        verification.function.parameters should have length 1
        verification.function.parameters.head.typeReference should be(a[TypeReference])
        verification.function.parameters.head.typeReference.asInstanceOf[TypeReference].typeName should be(result.name)
      }
    }
  }

  it should "return a type definition with defined list of verifications" in {
    forAll(TypeContextGenerator.anyDefinedTypeContext) { definedTypeContext =>
      val result = definitiASTParser.processDefinedType(definedTypeContext)

      Inspectors.forAll(result.verifications.toList) { verification =>
        verification.function.parameters should have length 1
        verification.function.parameters.head.typeReference should be(a[TypeReference])
        verification.function.parameters.head.typeReference.asInstanceOf[TypeReference].typeName should be(result.name)
      }
    }
  }

  it should "returns a type definition with defined list of verifications" in {
    val cases = for {
      definedTypeContext <- TypeContextGenerator.anyDefinedTypeContext
      numberOfVerifications <- Gen.posNum[Int]
      verificationNames <- Gen.listOfN(numberOfVerifications, Generators.anyIdentifier)
      verificationMessages <- Gen.listOfN(numberOfVerifications, Gen.option(Generators.anyString))
    } yield (definedTypeContext, verificationNames, verificationMessages)

    forAll(cases) { case (definedTypeContext, verificationNames, verificationMessages) =>
      val normalized = DefinedTypeContextMock(definedTypeContext).copy(
        verifyingListContext = Some(VerifyingListContextMock(
          verifyingContexts = verificationNames.zip(verificationMessages).map { case (name, message) =>
            VerifyingContextMock(TokenMock(name), message.map(TokenMock(_)))
          }
        ))
      )
      val result = definitiASTParser.processDefinedType(normalized)
      result.inherited should not be empty
      result.inherited.map(_.verificationName) should contain allElementsOf verificationNames
      result.inherited.map(_.message) should contain allElementsOf verificationMessages
    }
  }

  it should "returns a type definition with list of verifications if verifyingList is set" in {
    forAll(TypeContextGenerator.definedTypeContextWithVerifyingList) { attributeDefinitionContext =>
      val result = definitiASTParser.processDefinedType(attributeDefinitionContext)
      result.inherited should not be empty
    }
  }
}
