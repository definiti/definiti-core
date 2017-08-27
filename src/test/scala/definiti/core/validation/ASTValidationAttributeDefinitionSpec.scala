package definiti.core.validation

import definiti.core.generators.{ASTGenerator, ContextGenerator}
import definiti.core.parser.project.CoreParser
import definiti.core.{ConfigurationMock, Invalid, Valid}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ASTValidationAttributeDefinitionSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  private val configuration = ConfigurationMock()
  private val astValidation = new ASTValidation(configuration)

  "ASTValidation.validateAttributeDefinition" should "accept any attribute with a referenced type" in {
    val testGenerator = for {
      context <- ContextGenerator.anyContextWithNonEmptyVerifications(coreContext)
      attributeDefinition <- ASTGenerator.referencedAttributeDefinition(context)
    } yield {
      (context, attributeDefinition)
    }
    forAll(testGenerator) { case (context, attributeDefinition) =>
      astValidation.validateAttributeDefinition(attributeDefinition)(context) should be(Valid)
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
      astValidation.validateAttributeDefinition(attributeDefinition)(context) should be(an[Invalid])
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
      astValidation.validateAttributeDefinition(attributeDefinition)(context) should be(an[Invalid])
    }
  }
}
