package definiti.core.parser.project

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.ParserUtils._
import definiti.core.{HttpAST, Request, Requirement, _}

import scala.collection.mutable.ListBuffer

private[core] object HttpParser {
  def processHttp(context: HttpContext): HttpAST = {
    val requirements = ListBuffer[Requirement]()
    val requests = ListBuffer[Request]()

    scalaSeq(context.httpEntry()).foreach { httpEntry =>
      appendIfDefined(httpEntry.httpRequirement(), requirements, processRequirement)
      appendIfDefined(httpEntry.httpRequest(), requests, processRequest)
    }

    HttpAST(
      requirements = List(requirements: _*),
      requests = List(requests: _*),
      range = getRangeFromContext(context)
    )
  }

  def processRequirement(context: HttpRequirementContext): Requirement = {
    Requirement(
      name = context.name.getText,
      packageName = NOT_DEFINED,
      parameters = DefinitiASTParser.processParameterListDefinition(context.parameterListDefinition()),
      returnType = DefinitiASTParser.processTypeReference(context.typeReference()),
      comment = Option(context.DOC_COMMENT()).map(_.getText),
      range = getRangeFromContext(context)
    )
  }

  def processRequest(context: HttpRequestContext): Request = {
    Request(
      name = context.name.getText,
      input = processRequestInput(context.httpRequestInput()),
      requiring = Option(context.httpRequestRequiring()).map(processRequestRequiring).getOrElse(Seq.empty),
      returning = processRequestReturning(context.httpRequestReturning()),
      comment = Option(context.DOC_COMMENT()).map(_.getText),
      range = getRangeFromContext(context)
    )
  }

  def processRequestInput(context: HttpRequestInputContext): RequestInput = {
    RequestInput(
      method = context.httpVerb().getText,
      requestUri = processRequestURI(context.httpRequestURI()),
      inputType = Option(context.typeReference()).map(DefinitiASTParser.processTypeReference),
      range = getRangeFromContext(context)
    )
  }

  def processRequestURI(context: HttpRequestURIContext): RequestUri = {
    val query = Option(context.parameterListDefinition())
      .map(DefinitiASTParser.processParameterListDefinition)
      .getOrElse(Seq.empty)

    RequestUri(
      parts = scalaSeq(context.httpRequestURIPart()).map(processRequestURIPart),
      query = query,
      range = getRangeFromContext(context)
    )
  }

  def processRequestURIPart(context: HttpRequestURIPartContext): RequestUriPart = {
    if (context.STRING() != null) {
      FixedPart(
        text = context.STRING().getText,
        range = getRangeFromContext(context)
      )
    } else if (context.parameterDefinition() != null) {
      VariablePart(
        parameterDefinition = DefinitiASTParser.processParameter(context.parameterDefinition()),
        range = getRangeFromContext(context)
      )
    } else {
      // This exception exists to remind us to implement request URI part processing when we add one
      // This should never happen in production code.
      throw new RuntimeException(s"RequestURIPart ${context.getText} was not processed")
    }
  }

  def processRequestRequiring(context: HttpRequestRequiringContext): Seq[RequestRequirement] = {
    scalaSeq(context.httpRequestRequirement()).map(processRequestRequirement)
  }

  def processRequestRequirement(context: HttpRequestRequirementContext): RequestRequirement = {
    RequestRequirement(
      requirementReference = processRequirementReference(context.httpRequestRequirementReference()),
      returning = processRequestResult(context.httpResult()),
      range = getRangeFromContext(context)
    )
  }

  def processRequirementReference(context: HttpRequestRequirementReferenceContext): RequirementReference = {
    RequirementReference(
      name = context.name.getText,
      parameters = processParameterList(context.httpParameterList()),
      range = getRangeFromContext(context)
    )
  }

  def processParameterList(context: HttpParameterListContext): Seq[String] = {
    scalaSeq(context.httpParameter()).map(_.name.getText)
  }

  def processRequestReturning(context: HttpRequestReturningContext): Seq[RequestResult] = {
    scalaSeq(context.httpResult()).map(processRequestResult)
  }

  def processRequestResult(context: HttpResultContext): RequestResult = {
    RequestResult(
      status = extractStatusCode(context),
      output = extractRequestOutput(context),
      range = getRangeFromContext(context)
    )
  }

  def extractStatusCode(context: HttpResultContext): BigDecimal = {
    if (context.httpStatusNumber() != null) {
      BigDecimal(context.httpStatusNumber().NUMBER().getText)
    } else if (context.httpStatus() != null) {
      HttpStatus.httpStatuses.get(context.httpStatus().getText) match {
        case Some(status) => status
        case None => throw new RuntimeException(s"Invalid status ${context.httpStatus().getText}")
      }
    } else {
      // This exception exists to remind us to implement status code processing when we add one
      // This should never happen in production code.
      throw new RuntimeException(s"RequestURIPart ${context.getText} was not processed")
    }
  }

  def extractRequestOutput(context: HttpResultContext): Option[RequestOutput] = {
    lazy val typeReference = context.typeReference()
    if (context.raw != null) {
      Some(
        RawOutput(
          text = context.raw.getText,
          range = getRangeFromToken(context.raw)
        )
      )
    } else if (typeReference != null) {
      Some(
        ReferenceOutput(
          typeReference = DefinitiASTParser.processTypeReference(typeReference),
          range = getRangeFromContext(typeReference)
        )
      )
    } else {
      None
    }
  }
}
