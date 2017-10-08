package definiti.core.parser.project

import definiti.core.{_}
import definiti.core.ast._
import definiti.core.generators.antlr.ContextGenerator
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProcessContextSpec extends FlatSpec with Matchers with PropertyChecks {
  import ProcessContextSpec._

  private val configuration = ConfigurationMock(
    contexts = Seq(new ContextPluginTest)
  )
  private val definitiASTParser = new DefinitiASTParser("test.def", configuration)

  "DefinitiASTParser.processContext" should "return the result from plugin context when exists" in {
    forAll(ContextGenerator.anyContextOf(contextPluginName)) { contextContext =>
      val result = definitiASTParser.processContext(contextContext)

      result should not be empty
      result.foreach(_.name should ===(contextPluginName))
      result.foreach(_.content should be(a[ContextPluginTestContent]))
    }
  }
}

object ProcessContextSpec {
  val contextPluginName: String = "myContext"

  case class ContextPluginTestContent(content: String, location: Location)

  class ContextPluginTest extends ContextPlugin[ContextPluginTestContent] {
    override def contextName: String = contextPluginName

    override def parse(content: String, location: Location): ContextPluginTestContent = {
      ContextPluginTestContent(content, location)
    }

    override def validate(context: ContextPluginTestContent, library: Library): Validation = {
      Valid
    }

    override def name: String = "MyContextPlugin"
  }

}