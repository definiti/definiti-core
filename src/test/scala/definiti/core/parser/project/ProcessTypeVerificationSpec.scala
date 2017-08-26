package definiti.core.parser.project

import definiti.core.generators.antlr.TypeContextGenerator
import definiti.core.{ConfigurationMock, TypeReference}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProcessTypeVerificationSpec extends FlatSpec with Matchers with PropertyChecks {
  private val configuration = ConfigurationMock()
  private val definitiASTParser = new DefinitiASTParser(configuration)

  "DefinitiASTParser.processTypeVerification" should "returns a TypeVerification with a function of one parameter of given type" in {
    val cases = for {
      typeVerificationContext <- TypeContextGenerator.anyTypeVerificationContext
      typeName <- Gen.alphaNumStr
    } yield (typeVerificationContext, typeName)

    forAll(cases) { case (typeVerificationContext, typeName) =>
      val result = definitiASTParser.processTypeVerification(typeVerificationContext, typeName)
      result.function.parameters should have length 1
      result.function.parameters.head.typeReference should be (a[TypeReference])
      result.function.parameters.head.typeReference.asInstanceOf[TypeReference].typeName should be (typeName)
    }
  }
}
