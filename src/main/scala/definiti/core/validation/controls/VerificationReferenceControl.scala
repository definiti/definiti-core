package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.helpers.TypeReferenceControlHelper
import definiti.core.validation.{Control, ControlLevel, ControlResult}

object VerificationReferenceControl extends Control with TypeReferenceControlHelper {
  override val description: String = "Check if all verification references are valid (name and type)"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.projectTypes.collect {
        case aliasType: AliasType =>
          ControlResult.squash {
            aliasType.inherited.map(controlVerificationReference(_, aliasType, library))
          }
        case definedType: DefinedType =>
          ControlResult.squash {
            val inheritedControls = definedType.inherited.map(controlVerificationReference(_, definedType, library))
            val attributeControls = definedType.attributes.map(controlAttribute(_, library))
            inheritedControls ++ attributeControls
          }
      }
    }
  }

  private def controlVerificationReference(verificationReference: VerificationReference, expectedType: ClassDefinition, library: Library): ControlResult = {
    library.verificationsMap.get(verificationReference.verificationName) match {
      case Some(verification) => controlType(verification, expectedType, verificationReference.location, library)
      case None => ControlResult(errorUndefined(verificationReference.verificationName, verificationReference.location))
    }
  }

  private def controlType(verification: Verification, expectedType: ClassDefinition, location: Location, library: Library): ControlResult = {
    verification.function.parameters.headOption match {
      case Some(parameter) =>
        parameter.typeReference match {
          case typeReference: TypeReference => controlTypeReference(typeReference, expectedType, verification.fullName, location, library)
          case _ => ignored
        }
      case None => ignored
    }
  }

  private def controlTypeReference(typeReference: TypeReference, expectedType: ClassDefinition, verificationName: String, location: Location, library: Library): ControlResult = {
    val typesAreEqual = expectedType match {
      case aliasType: AliasType if typeReference.typeName != expectedType.fullName =>
        areTypeEqual(typeReference, aliasType.alias, library)
      case _ =>
        typeReference.typeName == expectedType.fullName
    }
    if (typesAreEqual) {
      OK
    } else {
      ControlResult(errorInvalidType(expectedType.fullName, verificationName, typeReference, location))
    }
  }

  private def controlAttribute(attribute: AttributeDefinition, library: Library): ControlResult = {
    attribute.typeReference match {
      case TypeReference(typeName, _) =>
        library.typesMap.get(typeName) match {
          case Some(classDefinition) =>
            ControlResult.squash {
              attribute.verifications.map(controlVerificationReference(_, classDefinition, library))
            }
          case None => ignored
        }
      case _ => ignored
    }
  }

  def errorUndefined(verificationName: String, location: Location): Alert = {
    alert(s"Undefined verification: ${verificationName}", location)
  }

  def errorInvalidType(typeName: String, verificationName: String, targetType: TypeReference, location: Location): Alert = {
    alert(
      s"Type ${typeName} inherit verification ${verificationName} which accept another type: ${targetType.readableString}",
      location
    )
  }
}
