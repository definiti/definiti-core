package definiti.core.typing

import definiti.core.ast.pure.{AliasType => PureAliasType, ClassDefinition => PureClassDefinition, DefinedType => PureDefinedType, NativeClassDefinition => PureNativeClassDefinition, TypeVerification => PureTypeVerification}
import definiti.core.ast.typed.{AliasType => TypedAliasType, ClassDefinition => TypedClassDefinition, DefinedType => TypedDefinedType, NativeClassDefinition => TypedNativeClassDefinition, TypeVerification => TypedTypeVerification}
import definiti.core.{Context, ValidValue, Validated}

class ClassDefinitionTyping(context: Context) {
  val functionTyping = new FunctionTyping(context)

  def addTypesIntoClassDefinition(classDefinition: PureClassDefinition): Validated[TypedClassDefinition] = {
    classDefinition match {
      case native: PureNativeClassDefinition => ValidValue(transformNativeClassDefinition(native))
      case definedType: PureDefinedType => addTypesIntoDefinedType(definedType)
      case aliasType: PureAliasType => ValidValue(transformAliasType(aliasType))
    }
  }

  def transformNativeClassDefinition(classDefinition: PureNativeClassDefinition): TypedNativeClassDefinition = {
    TypedNativeClassDefinition(
      name = classDefinition.name,
      genericTypes = classDefinition.genericTypes,
      attributes = classDefinition.attributes,
      methods = classDefinition.methods,
      comment = classDefinition.comment
    )
  }

  def addTypesIntoDefinedType(definedType: PureDefinedType): Validated[TypedDefinedType] = {
    val validatedTypeVerifications = Validated.squash(definedType.verifications.map(addTypesIntoTypeVerification))
    validatedTypeVerifications.map { typeVerifications =>
      TypedDefinedType(
        name = definedType.name,
        packageName = definedType.packageName,
        genericTypes = definedType.genericTypes,
        attributes = definedType.attributes,
        verifications = typeVerifications,
        inherited = definedType.inherited,
        comment = definedType.comment,
        range = definedType.range
      )
    }
  }

  def addTypesIntoTypeVerification(typeVerification: PureTypeVerification): Validated[TypedTypeVerification] = {
    val validatedFunction = functionTyping.addTypesIntoDefinedFunction(typeVerification.function)
    validatedFunction.map { function =>
      TypedTypeVerification(
        message = typeVerification.message,
        function = function,
        range = typeVerification.range
      )
    }
  }

  def transformAliasType(aliasType: PureAliasType): TypedAliasType = {
    TypedAliasType(
      name = aliasType.name,
      packageName = aliasType.packageName,
      genericTypes = aliasType.genericTypes,
      alias = aliasType.alias,
      inherited = aliasType.inherited,
      comment = aliasType.comment,
      range = aliasType.range
    )
  }
}
