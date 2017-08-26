package definiti.core.validation

import definiti.core._

private[core] class HttpValidation extends CommonValidation {
  def validateHttp(httpAST: HttpAST)(implicit context: Context): Validation = {
    val requirementValidations = httpAST.requirements.map(validateRequirement)
    val requestValidations = httpAST.requests.map(validateRequest)

    Validation.join(requirementValidations ++ requestValidations)
  }

  def validateRequirement(requirement: Requirement)(implicit context: Context): Validation = {
    val parameterValidations = requirement.parameters.map(validateParameterDefinition)
    val returnTypeValidation = validateTypeReference(requirement.returnType, requirement.range)
    Validation.join(parameterValidations :+ returnTypeValidation)
  }

  def validateRequest(request: Request)(implicit context: Context): Validation = {
    val inputValidation = validateRequestInput(request.input)
    val requiringValidation = request.requiring.map(validateRequestRequirement(_, request.input))
    val returningValidation = request.returning.map(validateRequestResult)
    Validation.join(inputValidation +: (requiringValidation ++ returningValidation))
  }

  def validateRequestInput(requestInput: RequestInput)(implicit context: Context): Validation = {
    val requestUriValidation = validateRequestUri(requestInput.requestUri)
    val inputTypeValidation = requestInput.inputType
      .map(inputType => validateTypeReference(inputType, requestInput.range))
      .getOrElse(Valid)
    Validation.join(requestUriValidation, inputTypeValidation)
  }

  def validateRequestUri(requestUri: RequestUri)(implicit context: Context): Validation = {
    val partValidations = requestUri.parts.map(validateRequestUriPart)
    val queryValidations = requestUri.query.map(validateParameterDefinition)
    Validation.join(partValidations ++ queryValidations)
  }

  def validateRequestUriPart(requestUriPart: RequestUriPart)(implicit context: Context): Validation = {
    requestUriPart match {
      case VariablePart(parameterDefinition, _) => validateParameterDefinition(parameterDefinition)
      case _ => Valid
    }
  }

  def validateRequestRequirement(requestRequirement: RequestRequirement, requestInput: RequestInput)(implicit context: Context): Validation = {
    val requirementReferenceValidation = validateRequirementReference(requestRequirement.requirementReference, requestInput)
    val returningValidation = validateRequestResult(requestRequirement.returning)
    Validation.join(requirementReferenceValidation, returningValidation)
  }

  def validateRequirementReference(requirementReference: RequirementReference, requestInput: RequestInput)(implicit context: Context): Validation = {
    context.findRequirement(requirementReference.name) match {
      case Some(requirement) =>
        if (requirementReference.parameters.length == requirement.parameters.length) {
          Validation.join(
            requirement.parameters.zip(requirementReference.parameters).map { case (requirementParameter, referenceParameter) =>
              extractVariableTypeReference(referenceParameter, requestInput) match {
                case Some(variableTypeReference) =>
                  if (isSameTypeReference(variableTypeReference, requirementParameter.typeReference)) {
                    Valid
                  } else {
                    Invalid(s"Expected type: ${requirementParameter.typeReference}, got: $variableTypeReference", requirementParameter.range)
                  }
                case None =>
                  Invalid(s"Undefined variable: $referenceParameter", requirementParameter.range)
              }
            }
          )
        } else {
          Invalid(s"Requirement ${requirementReference.name} except ${requirement.parameters.length} parameters, ${requirementReference.parameters.length} given", requirementReference.range)
        }
      case None =>
        Invalid("Undefined requirement: " + requirementReference.name, requirementReference.range)
    }
  }

  def extractVariableTypeReference(variableName: String, requestInput: RequestInput)(implicit context: Context): Option[TypeReference] = {
    def typeReferenceFromPart = requestInput.requestUri.parts.collect {
      case VariablePart(parameterDefinition, _) if parameterDefinition.name == variableName =>
        parameterDefinition.typeReference
    }.headOption

    def typeReferenceFromQuery = requestInput.requestUri.query
      .find(_.name == variableName)
      .map(_.typeReference)

    typeReferenceFromPart
      .orElse(typeReferenceFromQuery)
      .collect {
        case typeReference: TypeReference => typeReference
      }
  }

  def validateRequestResult(requestResult: RequestResult)(implicit context: Context): Validation = {
    requestResult.output
      .map {
        case _: RawOutput =>
          Valid
        case referenceOutput: ReferenceOutput =>
          validateTypeReference(referenceOutput.typeReference, referenceOutput.range)
      }
      .getOrElse(Valid)
  }
}
