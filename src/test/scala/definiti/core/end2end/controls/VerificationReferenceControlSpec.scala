package definiti.core.end2end.controls

import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.ast.Root
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.VerificationReferenceControl

class VerificationReferenceControlSpec extends EndToEndSpec {
  import VerificationReferenceControlSpec._

  "Project.generatePublicAST" should "validate a defined type inheriting a valid validation" in {
    val output = processFile("controls.verificationReference.validDefinedType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an alias type inheriting a valid validation" in {
    val output = processFile("controls.verificationReference.validAliasType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an attribute inheriting a valid validation for its native type" in {
    val output = processFile("controls.verificationReference.validAttributeNative", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an attribute inheriting a valid validation for its defined type" in {
    val output = processFile("controls.verificationReference.validAttributeDefinedType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an attribute inheriting a valid validation for its alias type" in {
    val output = processFile("controls.verificationReference.validAttributeAliasType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a defined type inheriting an invalid validation" in {
    val output = processFile("controls.verificationReference.invalidDefinedType", configuration)
    output should beKo(
      VerificationReferenceControl.errorUndefined("Unknown", invalidDefinedTypeLocation(1, 13, 30))
    )
  }

  it should "invalidate an alias type inheriting an invalid validation" in {
    val output = processFile("controls.verificationReference.invalidAliasType", configuration)
    output should beKo(
      VerificationReferenceControl.errorUndefined("Unknown", invalidAliasTypeLocation(1, 22, 39))
    )
  }

  it should "invalidate an attribute inheriting an invalid validation for its native type" in {
    val output = processFile("controls.verificationReference.invalidAttributeNative", configuration)
    output should beKo(
      VerificationReferenceControl.errorUndefined("Unknown", invalidAttributeNativeLocation(2, 16, 33))
    )
  }

  it should "invalidate an attribute inheriting an invalid validation for its defined type" in {
    val output = processFile("controls.verificationReference.invalidAttributeDefinedType", configuration)
    output should beKo(
      VerificationReferenceControl.errorUndefined("Unknown", invalidAttributeDefinedTypeLocation(7, 14, 31))
    )
  }

  it should "invalidate an attribute inheriting an invalid validation for its alias type" in {
    val output = processFile("controls.verificationReference.invalidAttributeAliasType", configuration)
    output should beKo(
      VerificationReferenceControl.errorUndefined("Unknown", invalidAttributeAliasTypeLocation(4, 14, 31))
    )
  }

  it should "invalidate a defined type inheriting an unmatched validation" in {
    val output = processFile("controls.verificationReference.unmatchedDefinedType", configuration)
    output should beKo(
      VerificationReferenceControl.errorInvalidType("MyType", "Unmatched", Constants.number, unmatchedDefinedTypeLocation(1, 13, 32))
    )
  }

  it should "invalidate an alias type inheriting an unmatched validation" in {
    val output = processFile("controls.verificationReference.unmatchedAliasType", configuration)
    output should beKo(
      VerificationReferenceControl.errorInvalidType("MyType", "Unmatched", Constants.number, unmatchedAliasTypeLocation(1, 22, 41))
    )
  }

  it should "invalidate an attribute inheriting an unmatched validation for its native type" in {
    val output = processFile("controls.verificationReference.unmatchedAttributeNative", configuration)
    output should beKo(
      VerificationReferenceControl.errorInvalidType("String", "Unmatched", Constants.number, unmatchedAttributeNativeLocation(2, 16, 35))
    )
  }

  it should "invalidate an attribute inheriting an unmatched validation for its defined type" in {
    val output = processFile("controls.verificationReference.unmatchedAttributeDefinedType", configuration)
    output should beKo(
      VerificationReferenceControl.errorInvalidType("Name", "Unmatched", Constants.number, unmatchedAttributeDefinedTypeLocation(7, 14, 33))
    )
  }

  it should "invalidate an attribute inheriting an unmatched validation for its alias type" in {
    val output = processFile("controls.verificationReference.unmatchedAttributeAliasType", configuration)
    output should beKo(
      VerificationReferenceControl.errorInvalidType("Name", "Unmatched", Constants.number, unmatchedAttributeAliasTypeLocation(4, 14, 33))
    )
  }
}

object VerificationReferenceControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(VerificationReferenceControl.name)

  val invalidDefinedTypeSrc = "src/test/resources/samples/controls/verificationReference/invalidDefinedType.def"
  val invalidDefinedTypeLocation = LocationPath(invalidDefinedTypeSrc)

  val invalidAliasTypeSrc = "src/test/resources/samples/controls/verificationReference/invalidAliasType.def"
  val invalidAliasTypeLocation = LocationPath(invalidAliasTypeSrc)

  val invalidAttributeNativeSrc = "src/test/resources/samples/controls/verificationReference/invalidAttributeNative.def"
  val invalidAttributeNativeLocation = LocationPath(invalidAttributeNativeSrc)

  val invalidAttributeDefinedTypeSrc = "src/test/resources/samples/controls/verificationReference/invalidAttributeDefinedType.def"
  val invalidAttributeDefinedTypeLocation = LocationPath(invalidAttributeDefinedTypeSrc)

  val invalidAttributeAliasTypeSrc = "src/test/resources/samples/controls/verificationReference/invalidAttributeAliasType.def"
  val invalidAttributeAliasTypeLocation = LocationPath(invalidAttributeAliasTypeSrc)

  val unmatchedDefinedTypeSrc = "src/test/resources/samples/controls/verificationReference/unmatchedDefinedType.def"
  val unmatchedDefinedTypeLocation = LocationPath(unmatchedDefinedTypeSrc)

  val unmatchedAliasTypeSrc = "src/test/resources/samples/controls/verificationReference/unmatchedAliasType.def"
  val unmatchedAliasTypeLocation = LocationPath(unmatchedAliasTypeSrc)

  val unmatchedAttributeNativeSrc = "src/test/resources/samples/controls/verificationReference/unmatchedAttributeNative.def"
  val unmatchedAttributeNativeLocation = LocationPath(unmatchedAttributeNativeSrc)

  val unmatchedAttributeDefinedTypeSrc = "src/test/resources/samples/controls/verificationReference/unmatchedAttributeDefinedType.def"
  val unmatchedAttributeDefinedTypeLocation = LocationPath(unmatchedAttributeDefinedTypeSrc)

  val unmatchedAttributeAliasTypeSrc = "src/test/resources/samples/controls/verificationReference/unmatchedAttributeAliasType.def"
  val unmatchedAttributeAliasTypeLocation = LocationPath(unmatchedAttributeAliasTypeSrc)
}