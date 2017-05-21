package definiti.core.parser

import definiti.core.generators.{ASTGenerator, NamedFunctionGenerator}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProjectLinkingInjectLinksIntoNamedFunctionSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  "ProjectLinking.injectLinksIntoNamedFunction" should "inject the package name into `NamedFunction`" in {
    implicit val context = coreContext
    val cases = for {
      namedFunction <- NamedFunctionGenerator.anyNamedFunctionWithoutPackage
      packageName <- ASTGenerator.anyIdentifier
    } yield (namedFunction, packageName)
    forAll(cases) { case (namedFunction, packageName) =>
      val result = ProjectLinking.injectLinksIntoNamedFunction(namedFunction, packageName, ProjectLinking.emptyTypeMapping)
      result.packageName should be(packageName)
    }
  }
}
