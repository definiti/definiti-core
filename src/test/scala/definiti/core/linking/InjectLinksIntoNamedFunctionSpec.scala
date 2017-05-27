package definiti.core.linking

import definiti.core.generators.{ASTGenerator, NamedFunctionGenerator}
import definiti.core.parser.project.CoreParser
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class InjectLinksIntoNamedFunctionSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  "ProjectLinking.injectLinksIntoNamedFunction" should "inject the package name into `NamedFunction`" in {
    implicit val context = coreContext
    val cases = for {
      namedFunction <- NamedFunctionGenerator.anyNamedFunctionWithoutPackage
      packageName <- ASTGenerator.anyIdentifier
    } yield (namedFunction, packageName)
    forAll(cases) { case (namedFunction, packageName) =>
      val result = ProjectLinking.injectLinksIntoNamedFunction(namedFunction, packageName, emptyTypeMapping)
      result.packageName should be(packageName)
    }
  }
}
