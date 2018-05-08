package definiti.core.typing

import definiti.common.ast._
import definiti.common.validation.{Valid, Validated}
import definiti.core.{Context, DefinedFunctionContext}

private[core] class ClassDefinitionTyping(context: Context) {
  def addTypesIntoClassDefinition(classDefinition: ClassDefinition): Validated[ClassDefinition] = {
    classDefinition match {
      case native: NativeClassDefinition => transformNativeClassDefinition(native)
      case definedType: DefinedType => addTypesIntoDefinedType(definedType)
      case aliasType: AliasType => addTypesIntoAliasType(aliasType)
      case enum: Enum => Valid(enum)
    }
  }

  def transformNativeClassDefinition(classDefinition: NativeClassDefinition): Validated[NativeClassDefinition] = {
    val validatedAttributes = Validated.squash(classDefinition.attributes.map(addTypesIntoAttributeDefinition))
    validatedAttributes.map { attributes =>
      classDefinition.copy(attributes = attributes)
    }
  }

  def addTypesIntoDefinedType(definedType: DefinedType): Validated[DefinedType] = {
    val validatedTypeVerifications = Validated.squash(definedType.verifications.map(addTypesIntoTypeVerification))
    val validatedAttributes = Validated.squash(definedType.attributes.map(addTypesIntoAttributeDefinition))
    val validatedInherited = Validated.squash(definedType.inherited.map(addTypesIntoVerificationReference))
    Validated.both(validatedTypeVerifications, validatedAttributes, validatedInherited)
      .map { case (typeVerifications, attributes, inherited) =>
        definedType.copy(
          attributes = attributes,
          verifications = typeVerifications,
          inherited = inherited
        )
      }
  }

  def addTypesIntoAttributeDefinition(attributeDefinition: AttributeDefinition): Validated[AttributeDefinition] = {
    val validatedTypeDeclaration = addTypesIntoTypeDeclaration(attributeDefinition.typeDeclaration)
    val validatedVerificationReferences = Validated.squash(attributeDefinition.verifications.map(addTypesIntoVerificationReference))
    Validated.both(validatedTypeDeclaration, validatedVerificationReferences).map { case (typeDeclaration, verifications) =>
      attributeDefinition.copy(
        typeDeclaration = typeDeclaration,
        verifications = verifications
      )
    }
  }

  def addTypesIntoTypeDeclaration(typeDeclaration: TypeDeclaration): Validated[TypeDeclaration] = {
    val expressionTyping = new ExpressionTyping(context)
    val validatedGenericTypes = Validated.squash(typeDeclaration.genericTypes.map(addTypesIntoTypeDeclaration))
    val validatedParameters = Validated.squash(typeDeclaration.parameters.map(expressionTyping.addTypeIntoAtomicExpression))
    Validated.both(validatedGenericTypes, validatedParameters).map { case (genericTypes, parameters) =>
      typeDeclaration.copy(
        genericTypes = genericTypes,
        parameters = parameters
      )
    }
  }

  def addTypesIntoVerificationReference(verificationReference: VerificationReference): Validated[VerificationReference] = {
    val expressionTyping = new ExpressionTyping(context)
    val validatedParameters = Validated.squash(verificationReference.parameters.map(expressionTyping.addTypeIntoAtomicExpression))
    validatedParameters.map { parameters =>
      verificationReference.copy(parameters = parameters)
    }
  }

  def addTypesIntoTypeVerification(typeVerification: TypeVerification): Validated[TypeVerification] = {
    typeVerification match {
      case atomicTypeVerification: AtomicTypeVerification =>
        val functionContext = DefinedFunctionContext(context, atomicTypeVerification.function)
        val validatedFunction = new FunctionTyping(functionContext).addTypesIntoDefinedFunction(atomicTypeVerification.function)
        validatedFunction.map { function =>
          atomicTypeVerification.copy(function = function)
        }

      case dependentTypeVerification: DependentTypeVerification =>
        val functionContext = DefinedFunctionContext(context, dependentTypeVerification.function)
        val validatedFunction = new FunctionTyping(functionContext).addTypesIntoDefinedFunction(dependentTypeVerification.function)
        validatedFunction.map { function =>
          dependentTypeVerification.copy(function = function)
        }
    }
  }

  def addTypesIntoAliasType(aliasType: AliasType): Validated[AliasType] = {
    val validatedAlias = addTypesIntoTypeDeclaration(aliasType.alias)
    val validatedTypeVerifications = Validated.squash(aliasType.verifications.map(addTypesIntoTypeVerification))
    val validatedInherited = Validated.squash(aliasType.inherited.map(addTypesIntoVerificationReference))
    Validated.both(validatedAlias, validatedTypeVerifications, validatedInherited)
      .map { case (alias, typeVerifications, inherited) =>
        aliasType.copy(
          alias = alias,
          verifications = typeVerifications,
          inherited = inherited
        )
      }
  }
}
