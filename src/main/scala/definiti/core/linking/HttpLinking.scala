package definiti.core.linking

import definiti.core._

private[core] object HttpLinking {
  def injectIntoHttp(http: HttpAST, packageName: String, typeMapping: TypeMapping): HttpAST = {
    http.copy(
      requirements = http.requirements.map { requirement =>
        injectIntoRequirement(requirement, packageName, typeMapping)
      },
      requests = http.requests.map { request =>
        injectIntoRequest(request, packageName, typeMapping)
      }
    )
  }

  def injectIntoRequirement(requirement: Requirement, packageName: String, typeMapping: TypeMapping): Requirement = {
    requirement.copy(
      packageName = packageName,
      parameters = requirement.parameters.map(ProjectLinking.injectLinksIntoParameter(_, typeMapping)),
      returnType = ProjectLinking.injectLinksIntoTypeReference(requirement.returnType, typeMapping)
    )
  }

  def injectIntoRequest(request: Request, packageName: String, typeMapping: TypeMapping): Request = {
    request.copy(
      packageName = packageName,
      input = injectIntoRequestInput(request.input, typeMapping),
      requiring = request.requiring.map(injectIntoRequestRequirement(_, typeMapping)),
      returning = request.returning.map(injectIntoRequestResult(_, typeMapping))
    )
  }

  def injectIntoRequestInput(requestInput: RequestInput, typeMapping: TypeMapping): RequestInput = {
    requestInput.copy(
      requestUri = injectIntoRequestUri(requestInput.requestUri, typeMapping),
      inputType = requestInput.inputType.map(ProjectLinking.injectLinksIntoTypeReference(_, typeMapping))
    )
  }

  def injectIntoRequestUri(requestUri: RequestUri, typeMapping: TypeMapping): RequestUri = {
    requestUri.copy(
      parts = requestUri.parts.map(injectIntoPart(_, typeMapping)),
      query = requestUri.query.map(ProjectLinking.injectLinksIntoParameter(_, typeMapping))
    )
  }

  def injectIntoPart(requestUriPart: RequestUriPart, typeMapping: TypeMapping): RequestUriPart = {
    requestUriPart match {
      case fixedPart: FixedPart => fixedPart
      case variablePart: VariablePart => variablePart.copy(
        parameterDefinition = ProjectLinking.injectLinksIntoParameter(variablePart.parameterDefinition, typeMapping)
      )
    }
  }

  def injectIntoRequestRequirement(requestRequirement: RequestRequirement, typeMapping: TypeMapping): RequestRequirement = {
    requestRequirement.copy(
      returning = injectIntoRequestResult(requestRequirement.returning, typeMapping)
    )
  }

  def injectIntoRequestResult(requestResult: RequestResult, typeMapping: TypeMapping): RequestResult = {
    requestResult.copy(
      output = requestResult.output.map(injectIntoRequestOutput(_, typeMapping))
    )
  }

  def injectIntoRequestOutput(requestOutput: RequestOutput, typeMapping: TypeMapping): RequestOutput = {
    requestOutput match {
      case rawOutput: RawOutput => rawOutput
      case referenceOutput: ReferenceOutput => referenceOutput.copy(
        typeReference = ProjectLinking.injectLinksIntoTypeReference(referenceOutput.typeReference, typeMapping)
      )
    }
  }
}