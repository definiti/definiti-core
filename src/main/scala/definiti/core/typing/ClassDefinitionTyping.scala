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
    val validatedTypeDeclaration = addTypesIntoTypeDeclaration(attributeDefinition.typeDeclaration)
    val validatedVerificationReferences = Validated.squash(attributeDefinition.verifications.map(addTypesIntoVerificationReference))
    Validated.both(validatedTypeDeclaration, validatedVerificationReferences).map { case (typeDeclaration, verifications) =>
      AttributeDefinition(
        name = attributeDefinition.name,
        typeDeclaration = typeDeclaration,
        comment = attributeDefinition.comment,
        verifications = verifications,
        location = attributeDefinition.location
      )
    }
  }

  def addTypesIntoTypeDeclaration(typeDeclaration: PureTypeDeclaration): Validated[TypeDeclaration] = {
    val expressionTyping = new ExpressionTyping(context)
    val validatedGenericTypes = Validated.squash(typeDeclaration.genericTypes.map(addTypesIntoTypeDeclaration))
    val validatedParameters = Validated.squash(typeDeclaration.parameters.map(expressionTyping.addTypeIntoAtomicExpression))
    Validated.both(validatedGenericTypes, validatedParameters).map { case (genericTypes, parameters) =>
      TypeDeclaration(
        typeName = typeDeclaration.typeName,
        genericTypes = genericTypes,
        parameters = parameters,
        location = typeDeclaration.location
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
    typeVerification match {
      case atomicTypeVerification: PureAtomicTypeVerification =>
        val functionContext = DefinedFunctionContext(context, atomicTypeVerification.function)
        val validatedFunction = new FunctionTyping(functionContext).addTypesIntoDefinedFunction(atomicTypeVerification.function)
        validatedFunction.map { function =>
          AtomicTypeVerification(
            message = atomicTypeVerification.message,
            function = function,
            location = atomicTypeVerification.location
          )
        }

      case dependentTypeVerification: PureDependentTypeVerification =>
        val functionContext = DefinedFunctionContext(context, dependentTypeVerification.function)
        val validatedFunction = new FunctionTyping(functionContext).addTypesIntoDefinedFunction(dependentTypeVerification.function)
        validatedFunction.map { function =>
          DependentTypeVerification(
            name = dependentTypeVerification.name,
            message = dependentTypeVerification.message,
            function = function,
            location = dependentTypeVerification.location
          )
        }
    }
  }

  def addTypesIntoAliasType(aliasType: PureAliasType): Validated[TypedAliasType] = {
    val validatedAlias = addTypesIntoTypeDeclaration(aliasType.alias)
    val validatedTypeVerifications = Validated.squash(aliasType.verifications.map(addTypesIntoTypeVerification))
    val validatedInherited = Validated.squash(aliasType.inherited.map(addTypesIntoVerificationReference))
    Validated.both(validatedAlias, validatedTypeVerifications, validatedInherited)
      .map { case (alias, typeVerifications, inherited) =>
        TypedAliasType(
          name = aliasType.name,
          packageName = aliasType.packageName,
          genericTypes = aliasType.genericTypes,
          parameters = aliasType.parameters,
          alias = alias,
          verifications = typeVerifications,
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
