package definiti.core.validation

import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.ast.structure.ClassDefinition
import definiti.core.ast.typed.Expression

private[core] trait CommonValidation {
  self: ASTValidation =>

  protected def validateParameterDefinition(parameterDefinition: ParameterDefinition): Validation = {
    validateAbstractTypeReference(parameterDefinition.typeReference, parameterDefinition.range)
  }

  protected def validateAbstractTypeReference(abstractTypeReference: AbstractTypeReference, range: Range): Validation = {
    abstractTypeReference match {
      case typeReference: TypeReference => validateTypeReference(typeReference, range)
      case _ => Valid
    }
  }

  protected def getReturnTypeName(expression: Expression): Validated[String] = {
    expression.returnType match {
      case typeReference: TypeReference =>
        if (library.types.contains(typeReference.typeName)) {
          ValidValue(typeReference.typeName)
        } else {
          Invalid("Undefined type: " + typeReference.readableString, expression.range)
        }
      case _ =>
        Invalid("Expected type reference, got lambda", expression.range)
    }
  }

  protected def getReturnType(expression: Expression): Validated[ClassDefinition] = {
    getReturnTypeName(expression).map(library.types(_))
  }

  protected def validateTypeReference(typeReference: TypeReference, range: Range): Validation = {
    val typeValidation =
      if (library.types.contains(typeReference.typeName)) {
        Valid
      } else {
        Invalid("Undefined type: " + typeReference.readableString, range)
      }

    val genericValidations = typeReference.genericTypes.map(validateTypeReference(_, range))

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
      case _ => Invalid("Expected boolean expression, got: class " + expression.returnType.readableString, expression.range)
    }
  }
}
