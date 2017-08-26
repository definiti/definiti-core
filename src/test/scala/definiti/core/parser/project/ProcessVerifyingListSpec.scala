package definiti.core.parser.project

import definiti.core.ConfigurationMock
import definiti.core.generators.antlr.VerificationContextGenerator
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class ProcessVerifyingListSpec extends FlatSpec with Matchers with PropertyChecks {
  private val configuration = ConfigurationMock()
  private val definitiASTParser = new DefinitiASTParser(configuration)

  "DefinitiASTParser.processVerifyingList" should "return VerificationReference without message when not set" in {
    forAll(VerificationContextGenerator.verifyingListContextWithoutMessage) { verifyingListContext =>
      val result = definitiASTParser.processVerifyingList(verifyingListContext)

      Inspectors.forAll(result.toList) { verificationReference =>
        verificationReference.message should be(empty)
      }
    }
  }

  "DefinitiASTParser.processVerifyingList" should "return VerificationReference with message when set" in {
    forAll(VerificationContextGenerator.verifyingListContextWithMessage) { verifyingListContext =>
      val result = definitiASTParser.processVerifyingList(verifyingListContext)

      Inspectors.forAll(result.toList) { verificationReference =>
        verificationReference.message should be(defined)
      }
    }
  }
}
