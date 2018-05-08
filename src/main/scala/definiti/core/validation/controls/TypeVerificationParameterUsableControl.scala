package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.TypeReferenceControlHelper

private[core] object TypeVerificationParameterUsableControl extends Control with TypeReferenceControlHelper {
  override val description: String = "Check if parameters in type verifications are valid"

  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      extractParameters(library).map(controlParameter(_, library))
    }
  }

  private def extractParameters(library: Library): Seq[ParameterInfo] = {
    val aliasTypeParameters = library.aliasTypes.flatMap(extractParametersFromAliasType)
    val definedTypeParameters = library.definedTypes.flatMap(extractParametersFromDefinedType)
    val namedFunctionTypeParameters = library.namedFunctions.flatMap(extractParametersFromNamedFunction)
    val verificationTypeParameters = library.verifications.flatMap(extractParametersFromVerification)
    aliasTypeParameters ++ definedTypeParameters ++ namedFunctionTypeParameters ++ verificationTypeParameters
  }

  private def extractParametersFromAliasType(aliasType: AliasType): Seq[ParameterInfo] = {
    val typeParameters = aliasType.parameters
      .map(ParameterInfo(_, aliasType.genericTypes))
    val verificationParameters = aliasType.verifications
      .flatMap(_.function.parameters)
      .map(ParameterInfo(_, aliasType.genericTypes))

    typeParameters ++ verificationParameters
  }

  private def extractParametersFromDefinedType(definedType: DefinedType): Seq[ParameterInfo] = {
    val typeParameters = definedType.parameters
      .map(ParameterInfo(_, definedType.genericTypes))
    val verificationParameters = definedType.verifications
      .flatMap(_.function.parameters)
      .map(ParameterInfo(_, definedType.genericTypes))

    typeParameters ++ verificationParameters
  }

  private def extractParametersFromNamedFunction(namedFunction: NamedFunction): Seq[ParameterInfo] = {
    namedFunction.parameters.map(ParameterInfo(_, namedFunction.genericTypes))
  }

  private def extractParametersFromVerification(verification: Verification): Seq[ParameterInfo] = {
    val verificationParameters = verification.parameters.map(ParameterInfo(_, Seq.empty))
    val functionParameters = verification.function.parameters.map(ParameterInfo(_, verification.function.genericTypes))
    verificationParameters ++ functionParameters
  }

  private def controlParameter(parameterInfo: ParameterInfo, library: Library): ControlResult = {
    parameterInfo.parameter.typeReference match {
      case typeReference: TypeReference =>
        controlTypeReference(typeReference, parameterInfo.generics, parameterInfo.parameter.location, library)
      case _ =>
        unexpectedTypeError(parameterInfo.parameter.typeReference, parameterInfo.parameter.location)
    }
  }

  def unexpectedTypeError(typeReference: AbstractTypeReference, location: Location): Alert = {
    alert(s"Unexpected type: ${typeReference.readableString}", location)
  }

  case class ParameterInfo(parameter: ParameterDefinition, generics: Seq[String])
}
