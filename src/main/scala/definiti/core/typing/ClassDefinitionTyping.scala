package definiti.core.typing

import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.ast.typed._
import definiti.core.{Context, DefinedFunctionContext, ValidValue, Validated}

private[core] class ClassDefinitionTyping(context: Context) {
  def addTypesIntoClassDefinition(classDefinition: PureClassDefinition): Validated[TypedClassDefinition] = {
    classDefinition match {
      case native: PureNativeClassDefinition => ValidValue(transformNativeClassDefinition(native))
      case definedType: PureDefinedType => addTypesIntoDefinedType(definedType)
      case aliasType: PureAliasType => addTypesIntoAliasType(aliasType)
      case enum: PureEnum => ValidValue(transformEnum(enum))
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
        location = definedType.location
      )
    }
  }

  def addTypesIntoTypeVerification(typeVerification: PureTypeVerification): Validated[TypeVerification] = {
    val functionContext = DefinedFunctionContext(context, typeVerification.function)
    val validatedFunction = new FunctionTyping(functionContext).addTypesIntoDefinedFunction(typeVerification.function)
    validatedFunction.map { function =>
      TypeVerification(
        message = typeVerification.message,
        function = function,
        location = typeVerification.location
      )
    }
  }

  def addTypesIntoAliasType(aliasType: PureAliasType): Validated[TypedAliasType] = {
    Validated.squash(aliasType.verifications.map(addTypesIntoTypeVerification)).map { typeVerifications =>
      TypedAliasType(
        name = aliasType.name,
        packageName = aliasType.packageName,
        genericTypes = aliasType.genericTypes,
        verifications = typeVerifications,
        alias = aliasType.alias,
        inherited = aliasType.inherited,
        comment = aliasType.comment,
        location = aliasType.location
      )
    }
  }

  def transformEnum(enum: PureEnum): TypedEnum = {
    TypedEnum(
      name = enum.name,
      packageName = enum.packageName,
      cases = enum.cases.map { enumCase =>
        TypedEnumCase(
          name = enumCase.name,
          comment = enumCase.comment,
          location = enumCase.location
        )
      },
      comment = enum.comment,
      location = enum.location
    )
  }
}
