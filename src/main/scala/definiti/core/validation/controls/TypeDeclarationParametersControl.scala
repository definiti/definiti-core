package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.{ParameterControlHelper, TypeReferenceControlHelper}

private[core] object TypeDeclarationParametersControl extends Control[Root] with ParameterControlHelper with TypeReferenceControlHelper {
  override val description: String = "Check if a type or attribute references another type with valid types"

  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      extractTypeDeclarations(library)
        .map(controlTypeDeclaration(_, library))
    }
  }

  private def extractTypeDeclarations(library: Library): Seq[TypeDeclaration] = {
    val aliasAliases = library.aliasTypes.map(_.alias)
    val definedAttributes = library.definedTypes.flatMap(_.attributes).map(_.typeDeclaration)

    aliasAliases ++ definedAttributes
  }

  private def controlTypeDeclaration(typeDeclaration: TypeDeclaration, library: Library): ControlResult = {
    library.typesMap.get(typeDeclaration.typeName) match {
      case Some(aliasType: AliasType) => controlTypeParameters(aliasType.parameters, typeDeclaration, library)
      case Some(definedType: DefinedType) => controlTypeParameters(definedType.parameters, typeDeclaration, library)
      case Some(_) => controlTypeParameters(Seq.empty, typeDeclaration, library)
      case None => unknownType(typeDeclaration.typeName, typeDeclaration.location)
    }
  }

  private def controlTypeParameters(expectedParameters: Seq[ParameterDefinition], typeDeclaration: TypeDeclaration, library: Library): ControlResult = {
    val expectedNumberOfParameters = expectedParameters.length
    val gotParameters = typeDeclaration.parameters
    val gotNumberOfParameters = gotParameters.length

    if (gotNumberOfParameters == expectedNumberOfParameters) {
      controlParameters(expectedParameters, gotParameters, library, typeDeclaration.location)
    } else {
      invalidNumberOfParameters(expectedNumberOfParameters, gotNumberOfParameters, typeDeclaration.location)
    }
  }

  def unknownType(typeName: String, location: Location): Alert = {
    alert(
      s"Unknown type ${typeName}",
      location
    )
  }
}
