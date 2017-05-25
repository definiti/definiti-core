package definiti.core.validation

import definiti.core.generators.{ASTGenerator, ContextGenerator}
import definiti.core.parser.project.CoreParser
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ASTValidationAttributeDefinitionSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  "ASTValidation.validateAttributeDefinition" should "accept any attribute with a referenced type" in {
    val testGenerator = for {
      context <- ContextGenerator.anyContextWithNonEmptyVerifications(coreContext)
      attributeDefinition <- ASTGenerator.referencedAttributeDefinition(context)
    } yield {
      (context, attributeDefinition)
    }
    forAll(testGenerator) { case (context, attributeDefinition) =>
      ASTValidation.validateAttributeDefinition(attributeDefinition)(context) should be(Valid)
    }
  }

  it should "refuse any attribute with a non-referenced type" in {
    val testGenerator = for {
      context <- ContextGenerator.anyContextWithNonEmptyVerifications(coreContext)
      attributeDefinition <- ASTGenerator.attributeDefinitionWithNonReferencedType(context)
    } yield {
      (context, attributeDefinition)
    }
    forAll(testGenerator) { case (context, attributeDefinition) =>
      ASTValidation.validateAttributeDefinition(attributeDefinition)(context) should be(an[Invalid])
    }
  }

  it should "refuse any attribute with a non-referenced verification" in {
    val testGenerator = for {
      context <- ContextGenerator.anyContextWithNonEmptyVerifications(coreContext)
      attributeDefinition <- ASTGenerator.attributeDefinitionWithNonReferencedVerification(context)
    } yield {
      (context, attributeDefinition)
    }
    forAll(testGenerator) { case (context, attributeDefinition) =>
      ASTValidation.validateAttributeDefinition(attributeDefinition)(context) should be(an[Invalid])
    }
  }
}
