package definiti.core.validation

import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._

trait CommonValidation {
  def validateParameterDefinition(parameterDefinition: ParameterDefinition)(implicit context: Context): Validation = {
    validateAbstractTypeReference(parameterDefinition.typeReference, parameterDefinition.range)
  }

  def validateAbstractTypeReference(abstractTypeReference: AbstractTypeReference, range: Range)(implicit context: Context): Validation = {
    abstractTypeReference match {
      case typeReference: TypeReference => validateTypeReference(typeReference, range)
      case _ => Valid
    }
  }

  def validateTypeReference(typeReference: TypeReference, range: Range)(implicit context: Context): Validation = {
    val typeValidation = if (context.isTypeAvailable(typeReference.typeName)) {
      Valid
    } else {
      Invalid("Undefined type: " + typeReference.typeName, range)
    }

    val genericValidations = typeReference.genericTypes.map(validateTypeReference(_, range))

    Validation.join(typeValidation +: genericValidations)
  }

  def isSameTypeReference(firstTypeReference: AbstractTypeReference, secondTypeReference: AbstractTypeReference): Boolean = {
    (firstTypeReference, secondTypeReference) match {
      case (firstLambdaReference: LambdaReference, secondLambdaReference: LambdaReference) =>
        lazy val sameNumberOfInputTypes = firstLambdaReference.inputTypes.length == secondLambdaReference.inputTypes.length
        lazy val sameInputTypes = firstLambdaReference.inputTypes.zip(secondLambdaReference.inputTypes).forall {
          case (firstGeneric, secondGeneric) => isSameTypeReference(firstGeneric, secondGeneric)
        }
        lazy val sameOutputType = isSameTypeReference(firstLambdaReference.outputType, secondLambdaReference.outputType)
        sameNumberOfInputTypes && sameInputTypes && sameOutputType
      case (firstTypeReference: TypeReference, secondTypeReference: TypeReference) =>
        lazy val sameType = firstTypeReference.typeName == secondTypeReference.typeName
        lazy val sameNumberOfGenerics = firstTypeReference.genericTypes.length == secondTypeReference.genericTypes.length
        lazy val sameGenerics = firstTypeReference.genericTypes.zip(secondTypeReference.genericTypes).forall {
          case (firstGeneric, secondGeneric) => isSameTypeReference(firstGeneric, secondGeneric)
        }
        sameType && sameNumberOfGenerics && sameGenerics
      case _ =>
        false
    }
  }
}
