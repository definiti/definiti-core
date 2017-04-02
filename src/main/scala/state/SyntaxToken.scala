package state

sealed trait SyntaxToken

sealed trait OpeningSyntax

sealed trait IgnoreEOLWithBrace

sealed trait ClosingSyntax {
  def opening: OpeningSyntax

  def enhancer(children: Seq[SyntaxToken]): EnhancedSyntaxToken
}

sealed trait Symbol extends SyntaxToken {
  def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken
}

case object EndOfLine extends SyntaxToken

case object Space extends SyntaxToken

case object OpenBrace extends SyntaxToken with OpeningSyntax

case object CloseBrace extends SyntaxToken with ClosingSyntax {
  override def opening: OpeningSyntax = OpenBrace

  override def enhancer(children: Seq[SyntaxToken]): EnhancedSyntaxToken = BraceExpressionToken(children)
}

case object OpenParenthesis extends SyntaxToken with OpeningSyntax

case object CloseParenthesis extends SyntaxToken with ClosingSyntax {
  override def opening: OpeningSyntax = OpenParenthesis

  override def enhancer(children: Seq[SyntaxToken]): EnhancedSyntaxToken = ParenthesisExpressionToken(children)
}

case object Colon extends SyntaxToken

case object Dot extends SyntaxToken with IgnoreEOLWithBrace

case object Underscore extends SyntaxToken

case object Comma extends SyntaxToken

case class LineComment(content: String) extends SyntaxToken

case class BlockComment(content: String) extends SyntaxToken

case class SymbolString(content: String) extends SyntaxToken

case class QuotedString(content: String) extends SyntaxToken

case class Word(content: String) extends SyntaxToken

case object VerificationKeyword extends SyntaxToken

case object VerifyKeyword extends SyntaxToken

case object VerifyingKeyword extends SyntaxToken

case object TypeKeyword extends SyntaxToken

case object IfKeyword extends SyntaxToken

case object ElseKeyword extends SyntaxToken

case object TrueKeyword extends SyntaxToken

case object FalseKeyword extends SyntaxToken

case object AssignSymbol extends SyntaxToken with IgnoreEOLWithBrace

case object EqualSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = EqualExpression(left, right)
}

case object NotSymbol extends SyntaxToken with IgnoreEOLWithBrace

case object NotEqualSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = NotEqualExpression(left, right)
}

case object LowerSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = LowerExpression(left, right)
}

case object UpperSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = UpperExpression(left, right)
}

case object LowerOrEqualSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = LowerOrEqualExpression(left, right)
}

case object UpperOrEqualSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = UpperOrEqualExpression(left, right)
}

case object MapSymbol extends SyntaxToken

case object AndSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = AndExpression(left, right)
}

case object OrSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = OrExpression(left, right)
}

case object PlusSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = PlusExpression(left, right)
}

case object MinusSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = MinusExpression(left, right)
}

case object TimeSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = TimeExpression(left, right)
}

case object DivideSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = DivideExpression(left, right)
}

case object ModuloSymbol extends SyntaxToken with IgnoreEOLWithBrace with Symbol {
  override def toExpressionToken(left: ExpressionToken, right: ExpressionToken): ExpressionToken = ModuloExpression(left, right)
}

case object Void extends SyntaxToken

case object Unknown extends SyntaxToken

sealed trait EnhancedSyntaxToken extends SyntaxToken

sealed trait ContainerToken[A <: EnhancedSyntaxToken] extends EnhancedSyntaxToken {
  def mapOnContainers(map: Seq[SyntaxToken] => Seq[SyntaxToken]): A
}

sealed trait EnclosingToken[A <: EnhancedSyntaxToken] extends ContainerToken[A] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): A = withChildren(map(children))

  def children: Seq[SyntaxToken]

  def withChildren(newChildren: Seq[SyntaxToken]): A
}

case class BraceExpressionToken(children: Seq[SyntaxToken]) extends EnclosingToken[BraceExpressionToken] {
  override def withChildren(newChildren: Seq[SyntaxToken]): BraceExpressionToken = BraceExpressionToken(newChildren)
}

case class ParenthesisExpressionToken(children: Seq[SyntaxToken]) extends EnclosingToken[ParenthesisExpressionToken] {
  override def withChildren(newChildren: Seq[SyntaxToken]): ParenthesisExpressionToken = ParenthesisExpressionToken(newChildren)
}

case class FunctionToken(parameters: Seq[FunctionParameter], body: BraceExpressionToken) extends ContainerToken[FunctionToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): FunctionToken = {
    FunctionToken(
      parameters,
      BraceExpressionToken(map(body.children))
    )
  }
}

case class FunctionParameter(name: String, typeReference: String) extends EnhancedSyntaxToken

case class ConditionToken(condition: ParenthesisExpressionToken, onTrue: BraceExpressionToken, onFalse: Option[BraceExpressionToken]) extends ContainerToken[ConditionToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): ConditionToken = {
    ConditionToken(
      ParenthesisExpressionToken(map(condition.children)),
      BraceExpressionToken(map(onTrue.children)),
      onFalse.map(body => BraceExpressionToken(map(body.children)))
    )
  }
}

case class VerificationToken(name: String, body: BraceExpressionToken) extends ContainerToken[VerificationToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): VerificationToken = {
    VerificationToken(name, body.mapOnContainers(map))
  }
}

case class TypeToken(name: String, definition: Either[String, BraceExpressionToken], verifications: Seq[String]) extends ContainerToken[TypeToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): TypeToken = {
    TypeToken(
      name,
      definition match {
        case Left(alias) => Left(alias)
        case Right(body) => Right(body.mapOnContainers(map))
      },
      verifications
    )
  }
}

case class StructuredVerificationToken(name: String, message: String, function: FunctionToken) extends ContainerToken[StructuredVerificationToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): StructuredVerificationToken = {
    StructuredVerificationToken(
      name,
      message,
      function.mapOnContainers(map)
    )
  }
}

sealed trait StructuredTypeToken extends EnhancedSyntaxToken

case class StructuredAliasTypeToken(name: String, alias: String, verifications: Seq[String]) extends StructuredTypeToken

case class StructuredDefinedTypeToken(name: String, fields: Seq[TypeFieldToken], definedVerifications: Seq[TypeVerificationToken], verifications: Seq[String]) extends StructuredTypeToken with ContainerToken[StructuredDefinedTypeToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): StructuredDefinedTypeToken = {
    StructuredDefinedTypeToken(
      name,
      fields,
      definedVerifications.map(_.mapOnContainers(map)),
      verifications
    )
  }
}

case class TypeFieldToken(name: String, typeReference: String) extends EnhancedSyntaxToken

case class TypeVerificationToken(message: String, function: FunctionToken) extends ContainerToken[TypeVerificationToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): TypeVerificationToken = {
    TypeVerificationToken(
      message,
      function.mapOnContainers(map)
    )
  }
}

sealed trait ExpressionToken extends EnhancedSyntaxToken

sealed trait SimpleExpressionToken extends ExpressionToken

case class BooleanExpressionToken(value: Boolean) extends SimpleExpressionToken

case class NumberExpressionToken(value: BigDecimal) extends SimpleExpressionToken

case class QuotedStringExpressionToken(value: String) extends SimpleExpressionToken

case class VariableExpressionToken(name: String) extends SimpleExpressionToken

case class MethodCallToken(expressionToken: ExpressionToken, method: String, parameters: ListExpressionToken) extends ExpressionToken

case class AttributeCallToken(expressionToken: ExpressionToken, attribute: String) extends ExpressionToken

case class ListExpressionToken(parts: Seq[ExpressionToken]) extends ExpressionToken

case class CombinedExpressionToken(parts: Seq[ExpressionToken]) extends ExpressionToken {
  def simplify(): ExpressionToken = parts match {
    case head :: Nil => head
    case _ => this
  }
}

case class ConditionExpressionToken(
  condition: ExpressionToken,
  onTrue: ExpressionToken,
  onFalse: Option[ExpressionToken]
) extends ExpressionToken

case class FunctionExpressionToken(parameters: Seq[FunctionParameter], body: ExpressionToken) extends EnhancedSyntaxToken

case class VerificationExpressionToken(name: String, message: String, function: FunctionExpressionToken) extends EnhancedSyntaxToken

case class DefinedTypeExpressionToken(name: String, fields: Seq[TypeFieldToken], definedVerifications: Seq[TypeVerificationExpressionToken], verifications: Seq[String]) extends EnhancedSyntaxToken

case class TypeVerificationExpressionToken(message: String, function: FunctionExpressionToken) extends EnhancedSyntaxToken

case class OrExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class AndExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class EqualExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class NotEqualExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class LowerExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class UpperExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class LowerOrEqualExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class UpperOrEqualExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class PlusExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class MinusExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class ModuloExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class TimeExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class DivideExpression(left: ExpressionToken, right: ExpressionToken) extends ExpressionToken

case class NotExpression(inner: ExpressionToken) extends ExpressionToken