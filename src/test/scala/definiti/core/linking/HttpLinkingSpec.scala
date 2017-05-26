package definiti.core.linking

import definiti.core.generators.{ASTGenerator, HttpASTGenerator, TypeMappingGenerator}
import definiti.core.parser.project.CoreParser
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class HttpLinkingSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  "HttpLinking.injectIntoHttp" should "inject package name into `HttpAST` without throwing exception" in {
    implicit val context = coreContext
    val cases = for {
      httpAST <- HttpASTGenerator.anyHttpAST
      packageName <- ASTGenerator.anyPackageName
      typeMapping <- TypeMappingGenerator.anyTypeMapping
    } yield (httpAST, packageName, typeMapping)
    forAll(cases) { case (httpAST, packageName, typeMapping) =>
      HttpLinking.injectIntoHttp(httpAST, packageName, typeMapping)
    }
  }

  "HttpLinking.injectIntoRequirements" should "have all Requirement with package name" in {
    implicit val context = coreContext
    val cases = for {
      requirement <- HttpASTGenerator.anyRequirement
      packageName <- ASTGenerator.anyPackageName
      typeMapping <- TypeMappingGenerator.anyTypeMapping
    } yield (requirement, packageName, typeMapping)
    forAll(cases) { case (requirement, packageName, typeMapping) =>
      val result = HttpLinking.injectIntoRequirement(requirement, packageName, typeMapping)
      result.packageName should equal(packageName)
    }
  }

  "HttpLinking.injectIntoRequest" should "have all Request with package name" in {
    implicit val context = coreContext
    val cases = for {
      request <- HttpASTGenerator.anyRequest
      packageName <- ASTGenerator.anyPackageName
      typeMapping <- TypeMappingGenerator.anyTypeMapping
    } yield (request, packageName, typeMapping)
    forAll(cases) { case (request, packageName, typeMapping) =>
      val result = HttpLinking.injectIntoRequest(request, packageName, typeMapping)
      result.packageName should equal(packageName)
    }
  }
}
