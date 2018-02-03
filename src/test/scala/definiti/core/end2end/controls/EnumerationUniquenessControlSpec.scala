package definiti.core.end2end.controls

import definiti.core.end2end.EndToEndSpec
import definiti.core.ProgramResultMatchers._
import definiti.core.ast.Root
import definiti.core.validation.controls.EnumerationUniquenessControl

class EnumerationUniquenessControlSpec extends EndToEndSpec {
  import EnumerationUniquenessControlSpec._

  "Project.generatePublicAST" should "validate an enum having all its cases once" in {
    val output = processFile("controls.enumerationUniqueness.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an enum with a case appearing twice" in {
    val output = processFile("controls.enumerationUniqueness.duplication", configuration)
    output should beKo(
      EnumerationUniquenessControl.errorDuplication("Monday", "Days", duplicationLocation(9, 3, 9))
    )
  }
}

object EnumerationUniquenessControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(EnumerationUniquenessControl.name)

  val duplicationSrc = "src/test/resources/samples/controls/enumerationUniqueness/duplication.def"
  val duplicationLocation = LocationPath(duplicationSrc)
}