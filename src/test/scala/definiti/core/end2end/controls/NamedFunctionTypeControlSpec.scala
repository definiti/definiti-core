package definiti.core.end2end.controls

import definiti.core.ast.Root
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.NamedFunctionTypeControl

class NamedFunctionTypeControlSpec extends EndToEndSpec {
  import NamedFunctionTypeControlSpec._
  import definiti.core.ProgramResultMatchers._

  "Project.generatePublicAST" should "validate the type of a named function when the body is the same" in {
    val output = processFile("controls.namedFunctionType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate the type of a named function in the case of the list of string" in {
    val output = processFile("controls.namedFunctionType.list", configuration)
    output shouldBe ok[Root]
  }

  it should "validate the type of a named function with a generic" in {
    val output = processFile("controls.namedFunctionType.generics", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate the type of a named function when types do not match" in {
    val output = processFile("controls.namedFunctionType.invalidType", configuration)
    output should beKo(
      NamedFunctionTypeControl.errorDifferentType("myFunction", "Number", "String", invalidTypeLocation(1, 1, 3, 2))
    )
  }

  it should "invalidate the type of a named function when types of list do not match" in {
    val output = processFile("controls.namedFunctionType.invalidList", configuration)
    output should beKo(
      NamedFunctionTypeControl.errorDifferentType("myFunction", "List[String]", "List[Number]", invalidListLocation(1, 1, 3, 2))
    )
  }

  it should "invalidate the type of a named function when types of list do not exist" in {
    val output = processFile("controls.namedFunctionType.invalidGenerics", configuration)
    output should beKo(
      NamedFunctionTypeControl.errorDifferentType("myFunction", "List[B]", "List[A]", invalidGenericsLocation(1, 1, 3, 2)),
      NamedFunctionTypeControl.errorUnknownType("B", "myFunction", invalidGenericsLocation(1, 1, 3, 2))
    )
  }

  it should "invalidate the type of a named function when it does not exist" in {
    val output = processFile("controls.namedFunctionType.unknownType", configuration)
    output should beKo(
      NamedFunctionTypeControl.errorDifferentType("myFunction", "Unknown", "String", invalidUnknownLocation(1, 1, 3, 2)),
      NamedFunctionTypeControl.errorUnknownType("Unknown", "myFunction", invalidUnknownLocation(1, 1, 3, 2))
    )
  }
}

object NamedFunctionTypeControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(NamedFunctionTypeControl.name)

  val invalidTypeSrc = "src/test/resources/samples/controls/namedFunctionType/invalidType.def"
  val invalidTypeLocation = LocationPath(invalidTypeSrc)

  val invalidListSrc = "src/test/resources/samples/controls/namedFunctionType/invalidList.def"
  val invalidListLocation = LocationPath(invalidListSrc)

  val invalidGenericsSrc = "src/test/resources/samples/controls/namedFunctionType/invalidGenerics.def"
  val invalidGenericsLocation = LocationPath(invalidGenericsSrc)

  val invalidUnknownSrc = "src/test/resources/samples/controls/namedFunctionType/unknownType.def"
  val invalidUnknownLocation = LocationPath(invalidUnknownSrc)
}