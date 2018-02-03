package definiti.core.validation.controls.helpers

import definiti.core.ast._
import definiti.core.validation.controls.{Control, ControlResult}
import definiti.core.{Alert, AlertControl}

trait TypeReferenceControlHelper {
  self: Control =>

  def controlTypeReference(typeReference: TypeReference, elementName: String, availableGenerics: Seq[String], location: Location, library: Library): ControlResult = {
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
        Seq(errorUnknownType(typeName, elementName, location))
      }
    }

    ControlResult(process(typeReference))
  }

  def errorUnknownType(typeName: String, elementName: String, location: Location): Alert = {
    AlertControl(
      name,
      s"Unknown type ${typeName} found in ${elementName}",
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
}
