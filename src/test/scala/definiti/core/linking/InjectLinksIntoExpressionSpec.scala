package definiti.core.linking

import definiti.core.ast.pure._
import definiti.core.generators.{ExpressionGenerator, TypeMappingGenerator}
import definiti.core.parser.project.CoreParser
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class InjectLinksIntoExpressionSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  "ProjectLinking.injectLinksIntoExpression" should "inject full name into `FunctionCall` without exception" in {
    implicit val context = coreContext
    val cases = for {
      functionCall <- ExpressionGenerator.anyFunctionCall
      typeMapping <- TypeMappingGenerator.anyTypeMapping
    } yield (functionCall, typeMapping)
    forAll(cases) { case (functionCall, typeMapping) =>
      ProjectLinking.injectLinksIntoExpression(functionCall, typeMapping)
    }
  }

  it should "not have any function call function with name in typeMapping values" in {
    implicit val context = coreContext
    val cases = for {
      functionCall <- ExpressionGenerator.anyFunctionCall
      typeMapping <- TypeMappingGenerator.anyTypeMapping
    } yield (functionCall, typeMapping)
    forAll(cases) { case (functionCall, typeMapping) =>
      val result = ProjectLinking.injectLinksIntoExpression(functionCall, typeMapping)
      result should be(an[PureFunctionCall])
      typeMapping should not contain result.asInstanceOf[PureFunctionCall].name
    }
  }
}
