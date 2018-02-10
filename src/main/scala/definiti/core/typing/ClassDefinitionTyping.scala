package definiti.core.typing

import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.ast.typed._
import definiti.core.{Context, DefinedFunctionContext, Valid, Validated}

private[core] class ClassDefinitionTyping(context: Context) {
  def addTypesIntoClassDefinition(classDefinition: PureClassDefinition): Validated[TypedClassDefinition] = {
    classDefinition match {
      case native: PureNativeClassDefinition => transformNativeClassDefinition(native)
      case definedType: PureDefinedType => addTypesIntoDefinedType(definedType)
      case aliasType: PureAliasType => addTypesIntoAliasType(aliasType)
      case enum: PureEnum => Valid(transformEnum(enum))
    }
  }

  def transformNativeClassDefinition(classDefinition: PureNativeClassDefinition): Validated[TypedNativeClassDefinition] = {
    val validatedAttributes = Validated.squash(classDefinition.attributes.map(addTypesIntoAttributeDefinition))
    validatedAttributes.map { attributes =>
      TypedNativeClassDefinition(
        name = classDefinition.name,
        genericTypes = classDefinition.genericTypes,
        attributes = attributes,
        methods = classDefinition.methods,
        comment = classDefinition.comment
      )
    }

  }

  def addTypesIntoDefinedType(definedType: PureDefinedType): Validated[TypedDefinedType] = {
    val validatedTypeVerifications = Validated.squash(definedType.verifications.map(addTypesIntoTypeVerification))
    val validatedAttributes = Validated.squash(definedType.attributes.map(addTypesIntoAttributeDefinition))
    val validatedInherited = Validated.squash(definedType.inherited.map(addTypesIntoVerificationReference))
    Validated.both(validatedTypeVerifications, validatedAttributes, validatedInherited)
      .map { case (typeVerifications, attributes, inherited) =>
        TypedDefinedType(
          name = definedType.name,
          packageName = definedType.packageName,
          genericTypes = definedType.genericTypes,
          parameters = definedType.parameters,
          attributes = attributes,
          verifications = typeVerifications,
          inherited = inherited,
          comment = definedType.comment,
          location = definedType.location
        )
      }
  }

  def addTypesIntoAttributeDefinition(attributeDefinition: PureAttributeDefinition): Validated[AttributeDefinition] = {
    val validatedVerificationReferences = Validated.squash(attributeDefinition.verifications.map(addTypesIntoVerificationReference))
    validatedVerificationReferences.map { verifications =>
      AttributeDefinition(
        name = attributeDefinition.name,
        typeReference = attributeDefinition.typeReference,
        comment = attributeDefinition.comment,
        verifications = verifications,
        location = attributeDefinition.location
      )
    }
  }

  def addTypesIntoVerificationReference(verificationReference: PureVerificationReference): Validated[VerificationReference] = {
    val expressionTyping = new ExpressionTyping(context)
    val validatedParameters = Validated.squash(verificationReference.parameters.map(expressionTyping.addTypeIntoAtomicExpression))
    validatedParameters.map { expressions =>
      VerificationReference(
        verificationName = verificationReference.verificationName,
        parameters = expressions,
        location = verificationReference.location
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
    val validatedTypeVerifications = Validated.squash(aliasType.verifications.map(addTypesIntoTypeVerification))
    val validatedInherited = Validated.squash(aliasType.inherited.map(addTypesIntoVerificationReference))
    Validated.both(validatedTypeVerifications, validatedInherited)
      .map { case (typeVerifications, inherited) =>
        TypedAliasType(
          name = aliasType.name,
          packageName = aliasType.packageName,
          genericTypes = aliasType.genericTypes,
          parameters = aliasType.parameters,
          verifications = typeVerifications,
          alias = aliasType.alias,
          inherited = inherited,
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
