package definiti.core.validation.controls.aliasType

import definiti.common.ast._
import definiti.common.control._
import definiti.common.validation._

private[core] object MethodAcceptationControl extends Control[Root] {
  override val description: String = "Check if the alias type can accept a method"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    library.aliasTypes.map(controlAliasType(_, library))
  }

  private def controlAliasType(aliasType: AliasType, library: Library): ControlResult = {
    aliasType.kind match {
      case AliasTypeKind.Opaque => OK
      case _ => ControlResult(aliasType.methods.map(method => errorMethodNotAccepted(aliasType.kind, method.location)))
    }
  }

  def errorMethodNotAccepted(aliasTypeKind: AliasTypeKind.Value, location: Location): Alert = {
    alert(s"You cannot declare a method for a ${aliasTypeKind} type. If you need to define a method, consider Opaque type instead", location)
  }
}
