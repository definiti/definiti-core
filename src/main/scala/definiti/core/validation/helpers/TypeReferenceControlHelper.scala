package definiti.core.validation.helpers

import definiti.common.ast._
import definiti.common.control.{Control, ControlResult}
import definiti.common.validation.Alert

private[core] trait TypeReferenceControlHelper {
  self: Control[Root] =>

  def controlTypeReference(typeReference: TypeReference, availableGenerics: Seq[String], location: Location, library: Library): ControlResult = {
    def process(typeReference: TypeReference): Seq[Alert] = {
      val typeNameAlerts = controlTypeName(typeReference.typeName)
      val genericAlerts = typeReference.genericTypes.flatMap(process)
      typeNameAlerts ++ genericAlerts
    }

    def controlTypeName(typeName: String): Seq[Alert] = {
      if (library.typesMap.contains(typeName)) {
        Seq.empty
      } else if (availableGenerics.contains(typeName)) {
        Seq.empty
      } else {
        Seq(errorUnknownType(typeName, location))
      }
    }

    ControlResult(process(typeReference))
  }

  def errorUnknownType(typeName: String, location: Location): Alert = {
    alert(
      s"Unknown type ${typeName}",
      location
    )
  }

  def isBoolean(typeReference: AbstractTypeReference, library: Library): Boolean = {
    typeReference match {
      case typeReference: TypeReference =>
        library.typesMap.get(typeReference.typeName) match {
          case Some(native: NativeClassDefinition) => native.name == "Boolean"
          case Some(alias: AliasType) => isBoolean(alias.alias, library)
          case _ => false
        }
      case _ => false
    }
  }

  def isNumber(typeReference: AbstractTypeReference, library: Library): Boolean = {
    typeReference match {
      case typeReference: TypeReference =>
        library.typesMap.get(typeReference.typeName) match {
          case Some(native: NativeClassDefinition) => native.name == "Number"
          case Some(alias: AliasType) => isNumber(alias.alias, library)
          case _ => false
        }
      case _ => false
    }
  }

  def replaceGenerics(typeReference: TypeReference, referenceTypeReference: TypeReference): AbstractTypeReference = {
    typeReference.copy(genericTypes = referenceTypeReference.genericTypes)
  }

  def areTypeEqual(expectedType: AbstractTypeReference, gotType: AbstractTypeReference, library: Library): Boolean = {
    normalizeType(expectedType, library) == normalizeType(gotType, library)
  }

  private def normalizeType(abstractTypeReference: AbstractTypeReference, library: Library): AbstractTypeReference = {
    abstractTypeReference match {
      case typeReference: TypeReference =>
        getRealType(typeReference, library)
      case lambdaReference: LambdaReference =>
        LambdaReference(
          inputTypes = lambdaReference.inputTypes.map(getRealType(_, library)),
          outputType = getRealType(lambdaReference.outputType, library)
        )
      case _ => abstractTypeReference
    }
  }

  def getRealType(typeReference: TypeReference, library: Library): TypeReference = {
    def process(typeReference: TypeReference): TypeReference = {
      library.typesMap.get(typeReference.typeName) match {
        case Some(aliasType: AliasType) => process(aliasType.alias)
        case _ => TypeReference(
          typeName = typeReference.typeName,
          genericTypes = typeReference.genericTypes.map(process)
        )
      }
    }
    process(typeReference)
  }

  def controlTypeEquality(expectedType: AbstractTypeReference, gotType: AbstractTypeReference, location: Location, library: Library): ControlResult = {
    if (areTypeEqual(expectedType, gotType, library)) {
      OK
    } else {
      errorTypeEquality(expectedType, gotType, location)
    }
  }

  def errorTypeEquality(expected: AbstractTypeReference, got: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Expected type ${expected.readableString}, got ${got.readableString}",
      location
    )
  }

  implicit protected def typeDeclarationToTypeReference(typeDeclaration: TypeDeclaration): TypeReference = {
    TypeReference(
      typeName = typeDeclaration.typeName,
      genericTypes = typeDeclaration.genericTypes.map(typeDeclarationToTypeReference)
    )
  }
}
