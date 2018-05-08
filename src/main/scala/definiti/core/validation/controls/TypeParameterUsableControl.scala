package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.TopLevelParameterControlHelper

private[core] object TypeParameterUsableControl extends Control with TopLevelParameterControlHelper {
  override val description: String = "Check if parameter types in alias or defined type are valid"

  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      extractParameters(library)
        .map(controlParameter(_, library))
    }
  }

  private def extractParameters(library: Library): Seq[ParameterInfo] = {
    val aliasTypeParameters = library.aliasTypes.flatMap { aliasType =>
      aliasType.parameters.map(ParameterInfo(aliasType.fullName, _))
    }
    val definedTypeParameters = library.definedTypes.flatMap { aliasType =>
      aliasType.parameters.map(ParameterInfo(aliasType.fullName, _))
    }
    aliasTypeParameters ++ definedTypeParameters
  }
}
