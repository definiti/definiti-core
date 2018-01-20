package definiti.core.end2end

import definiti.core.ValidValue
import definiti.core.ast.Root

class ParameterTypesSpec extends EndToEndSpec {
  import definiti.core.ValidationMatchers._
  import ParameterTypesSpec._

  "The generator" should "Accept a verification with a parameter" in {
    val expected = ValidValue(verification)
    val output = processFile("parameterTypes.verification")
    output should beValidated[Root](expected)
  }
}

object ParameterTypesSpec {
  val verificationFile = ""
  val verification = Root(Seq.empty)
}
