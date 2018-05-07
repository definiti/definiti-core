package definiti.core.end2end

import definiti.common._
import definiti.common.ast._
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation._
import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.mock.plugins.StringExtendedContext

class ExtendedContextSpec extends EndToEndSpec {
  import ExtendedContextSpec._

  "Project.generatePublicAST" should "be valid when extended context is valid on root package" in {
    val output = processFile("extendedContext.nominal", configuration(ValidExtendedContext))
    output shouldBe ok[Root]
  }

  it should "be invalid when extended context is valid on root package" in {
    val output = processFile("extendedContext.nominal", configuration(InvalidExtendedContext))
    output should beKo(givenError)
  }

  it should "be valid when extended context is valid on non-root package" in {
    val output = processFile("extendedContext.namespace", configuration(ValidExtendedContext))
    output shouldBe ok[Root]
  }

  it should "be invalid when extended context is valid on non-root package" in {
    val output = processFile("extendedContext.namespace", configuration(InvalidExtendedContext))
    output should beKo(givenError)
  }
}

object ExtendedContextSpec {

  val givenError: Alert = AlertSimple("error")

  def configuration(context: StringExtendedContext): ConfigurationMock = {
    ConfigurationMock(
      contexts = Seq(context)
    )
  }

  object ValidExtendedContext extends StringExtendedContext {
    override def validate(context: String, library: Library) = Valid(NoResult)
  }

  object InvalidExtendedContext extends StringExtendedContext {
    override def validate(context: String, library: Library) = Invalid(Seq(SimpleError(givenError.prettyPrint)))
  }

}