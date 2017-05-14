package definiti.core.parser

import definiti.core.generators.antlr.TypeContextGenerator
import definiti.core.TypeReference
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class DefinitiASTParserProcessDefinedTypeSpec extends FlatSpec with Matchers with PropertyChecks {
  "DefinitiASTParser.processDefinedType" should "have only TypeVerification with DefinedTypeName" in {
    forAll(TypeContextGenerator.anyDefinedTypeContext) { definedTypeContext =>
      val result = DefinitiASTParser.processDefinedType(definedTypeContext)

      Inspectors.forAll(result.verifications.toList) { verification =>
        verification.function.parameters should have length 1
        verification.function.parameters.head.typeReference should be(a[TypeReference])
        verification.function.parameters.head.typeReference.asInstanceOf[TypeReference].typeName should be(result.name)
      }
    }
  }
}
