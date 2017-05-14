package definiti.core.parser

import definiti.core.TypeReference
import definiti.core.generators.antlr.TypeContextGenerator
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class DefinitiASTParserProcessTypeVerificationSpec extends FlatSpec with Matchers with PropertyChecks {
  "DefinitiASTParser.processTypeVerification" should "returns a TypeVerification with a function of one parameter of given type" in {
    val cases = for {
      typeVerificationContext <- TypeContextGenerator.anyTypeVerificationContext
      typeName <- Gen.alphaNumStr
    } yield (typeVerificationContext, typeName)

    forAll(cases) { case (typeVerificationContext, typeName) =>
      val result = DefinitiASTParser.processTypeVerification(typeVerificationContext, typeName)
      result.function.parameters should have length 1
      result.function.parameters.head.typeReference should be (a[TypeReference])
      result.function.parameters.head.typeReference.asInstanceOf[TypeReference].typeName should be (typeName)
    }
  }
}
