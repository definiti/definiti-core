package definiti.core.validation

import definiti.core._
import definiti.core.ast.{Expression, _}

private[core] trait CommonValidation {
  self: ASTValidation =>

  protected def getReturnTypeName(expression: Expression): Validated[String] = {
    expression.returnType match {
      case typeReference: TypeReference =>
        if (library.types.contains(typeReference.typeName)) {
          ValidValue(typeReference.typeName)
        } else {
          Invalid("Undefined type: " + typeReference.readableString, expression.location)
        }
      case _ =>
        Invalid("Expected type reference, got lambda", expression.location)
    }
  }

  protected def getReturnType(expression: Expression): Validated[ClassDefinition] = {
    getReturnTypeName(expression).map(library.types(_))
  }

  protected def validateTypeReference(typeReference: TypeReference, genericTypes: Seq[String], location: Location): Validation = {
    val typeValidation =
      if (genericTypes.contains(typeReference.typeName)) {
        Valid
      } else if (library.types.contains(typeReference.typeName)) {
        Valid
      } else {
        Invalid("Undefined type: " + typeReference.readableString, location)
      }

    val genericValidations = typeReference.genericTypes.map(validateTypeReference(_, genericTypes, location))

    Validation.join(typeValidation +: genericValidations)
  }

  protected def isSameTypeReference(firstTypeReference: AbstractTypeReference, secondTypeReference: AbstractTypeReference): Boolean = {
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

  protected def validateBooleanExpression(expression: Expression): Validation = {
    expression.returnType match {
      case TypeReference(BOOLEAN, Seq()) => Valid
      case _ => Invalid("Expected boolean expression, got: class " + expression.returnType.readableString, expression.location)
    }
  }
}
