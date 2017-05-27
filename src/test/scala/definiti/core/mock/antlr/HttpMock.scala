package definiti.core.mock.antlr

import java.util

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class HttpContextMock(
  httpEntries: Seq[HttpEntryContext]
) extends HttpContext(null, 0) {
  override def httpEntry(): util.List[HttpEntryContext] = javaList(httpEntries)

  override def httpEntry(i: Int): HttpEntryContext = httpEntries(i)
}

case class HttpEntryContextRequirementMock(
  httpRequirementContext: HttpRequirementContext
) extends HttpEntryContext(null, 0) {
  override def httpRequirement(): HttpRequirementContext = httpRequirementContext

  override def httpRequest(): HttpRequestContext = null
}

case class HttpEntryContextRequestMock(
  httpRequestContext: HttpRequestContext
) extends HttpEntryContext(null, 0) {
  override def httpRequirement(): HttpRequirementContext = null

  override def httpRequest(): HttpRequestContext = httpRequestContext
}

case class HttpRequirementContextMock(
  nameToken: Token,
  parameterListDefinitionContext: ParameterListDefinitionContext,
  typeReferenceContext: TypeReferenceContext,
  docComment: TerminalNode
) extends HttpRequirementContext(null, 0) {
  this.name = nameToken

  override def parameterListDefinition(): ParameterListDefinitionContext = parameterListDefinitionContext

  override def typeReference(): TypeReferenceContext = typeReferenceContext

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(nameToken)

  override def DOC_COMMENT(): TerminalNode = docComment
}

case class HttpRequestContextMock(
  nameToken: Token,
  httpRequestInputContext: HttpRequestInputContext,
  httpRequestRequiringContext: HttpRequestRequiringContext,
  httpRequestReturningContext: HttpRequestReturningContext,
  docComment: TerminalNode
) extends HttpRequestContext(null, 0) {
  this.name = nameToken

  override def httpRequestInput(): HttpRequestInputContext = httpRequestInputContext

  override def httpRequestReturning(): HttpRequestReturningContext = httpRequestReturningContext

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(nameToken)

  override def DOC_COMMENT(): TerminalNode = docComment

  override def httpRequestRequiring(): HttpRequestRequiringContext = httpRequestRequiringContext
}

case class HttpRequestInputContextMock(
  httpVerbContext: HttpVerbContext,
  httpRequestURIContext: HttpRequestURIContext,
  typeReferenceContext: TypeReferenceContext
) extends HttpRequestInputContext(null, 0) {
  override def httpVerb(): HttpVerbContext = httpVerbContext

  override def httpRequestURI(): HttpRequestURIContext = httpRequestURIContext

  override def typeReference(): TypeReferenceContext = typeReferenceContext
}

object HttpVerbs extends Enumeration {
  val CONNECT, DELETE, GET, HEAD, PATCH, POST, PUT, OPTIONS, TRACE = Value
}
case class HttpVerbContextMock(
  verb: HttpVerbs.Value
) extends HttpVerbContext(null, 0) {
  override def getText: String = verb.toString
}

case class HttpRequestURIContextMock(
  httpRequestURIPartContexts: Seq[HttpRequestURIPartContext],
  parameterListDefinitionContext: ParameterListDefinitionContext
) extends HttpRequestURIContext(null, 0) {
  override def httpRequestURIPart(): util.List[HttpRequestURIPartContext] = javaList(httpRequestURIPartContexts)

  override def httpRequestURIPart(i: Int): HttpRequestURIPartContext = httpRequestURIPartContexts(i)

  override def parameterListDefinition(): ParameterListDefinitionContext = parameterListDefinitionContext
}

case class HttpRequestURIPartContextMock(
  value: Either[TerminalNode, ParameterDefinitionContext]
) extends HttpRequestURIPartContext(null, 0) {
  override def STRING(): TerminalNode = value.left.toOption.orNull

  override def parameterDefinition(): ParameterDefinitionContext = value.right.toOption.orNull
}

case class HttpRequestRequiringContextMock(
  httpRequestRequirementContexts: Seq[HttpRequestRequirementContext]
) extends HttpRequestRequiringContext(null, 0) {
  override def httpRequestRequirement(): util.List[HttpRequestRequirementContext] = javaList(httpRequestRequirementContexts)

  override def httpRequestRequirement(i: Int): HttpRequestRequirementContext = httpRequestRequirementContexts(i)
}

case class HttpRequestRequirementContextMock(
  httpRequestRequirementReferenceContext: HttpRequestRequirementReferenceContext,
  httpResultContext: HttpResultContext
) extends HttpRequestRequirementContext(null, 0) {
  override def httpRequestRequirementReference(): HttpRequestRequirementReferenceContext = httpRequestRequirementReferenceContext

  override def httpResult(): HttpResultContext = httpResultContext
}

case class HttpRequestRequirementReferenceContextMock(
  nameToken: Token,
  httpParameterListContext: HttpParameterListContext
) extends HttpRequestRequirementReferenceContext(null, 0) {
  this.name = nameToken

  override def httpParameterList(): HttpParameterListContext = httpParameterListContext

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(nameToken)
}

case class HttpParameterListContextMock(
  httpParameterContexts: Seq[HttpParameterContext]
) extends HttpParameterListContext(null, 0) {
  override def httpParameter(): util.List[HttpParameterContext] = javaList(httpParameterContexts)

  override def httpParameter(i: Int): HttpParameterContext = httpParameterContexts(i)
}

case class HttpParameterContextMock(
  nameToken: Token
) extends HttpParameterContext(null, 0) {
  this.name = nameToken

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(nameToken)
}

case class HttpRequestReturningContextMock(
  httpResultContexts: Seq[HttpResultContext]
) extends HttpRequestReturningContext(null, 0) {
  override def httpResult(): util.List[HttpResultContext] = javaList(httpResultContexts)

  override def httpResult(i: Int): HttpResultContext = httpResultContexts(i)
}

case class HttpResultContextMock(
  status: Either[HttpStatusNumberContext, HttpStatusContext],
  result: Option[Either[Token, TypeReferenceContext]]
) extends HttpResultContext(null, 0) {
  this.raw = result.flatMap(_.left.toOption).orNull

  override def httpStatus(): HttpStatusContext = status.right.toOption.orNull

  override def httpStatusNumber(): HttpStatusNumberContext = status.left.toOption.orNull

  override def typeReference(): TypeReferenceContext = result.flatMap(_.right.toOption).orNull

  override def STRING(): TerminalNode = Option(raw).map(TerminalNodeMock(_)).orNull
}

case class HttpStatusNumberContextMock(
  number: BigDecimal
) extends HttpStatusNumberContext(null, 0) {
  override def NUMBER(): TerminalNode = TerminalNodeMock(number.toString())
}

case class HttpStatusContextMock(
  status: String
) extends HttpStatusContext(null, 0) {
  override def getText: String = status
}