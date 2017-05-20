package definiti.core.parser

import definiti.core.generators.antlr.VerificationContextGenerator
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class DefinitiASTParserProcessVerifyingListSpec extends FlatSpec with Matchers with PropertyChecks {
  "DefinitiASTParser.processVerifyingList" should "return VerificationReference without message when not set" in {
    forAll(VerificationContextGenerator.verifyingListContextWithoutMessage) { verifyingListContext =>
      val result = DefinitiASTParser.processVerifyingList(verifyingListContext)

      Inspectors.forAll(result.toList) { verificationReference =>
        verificationReference.message should be(empty)
      }
    }
  }

  "DefinitiASTParser.processVerifyingList" should "return VerificationReference with message when set" in {
    forAll(VerificationContextGenerator.verifyingListContextWithMessage) { verifyingListContext =>
      val result = DefinitiASTParser.processVerifyingList(verifyingListContext)

      Inspectors.forAll(result.toList) { verificationReference =>
        verificationReference.message should be(defined)
      }
    }
  }
}
