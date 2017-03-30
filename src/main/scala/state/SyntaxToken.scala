package state

sealed trait SyntaxToken

sealed trait OpeningSyntax

sealed trait ClosingSyntax {
  def opening: OpeningSyntax

  def enhancer(children: Seq[SyntaxToken]): EnhancedSyntaxToken
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

case object Dot extends SyntaxToken

case object Underscore extends SyntaxToken

case object Comma extends SyntaxToken

case class Symbol(content: String) extends SyntaxToken

case class QuotedString(content: String) extends SyntaxToken

case class Word(content: String) extends SyntaxToken

case object VerificationKeyword extends SyntaxToken

case object VerifyKeyword extends SyntaxToken

case object VerifyingKeyword extends SyntaxToken

case object TypeKeyword extends SyntaxToken

case object Void extends SyntaxToken

case object Unknown extends SyntaxToken

sealed trait EnhancedSyntaxToken extends SyntaxToken

sealed trait EnclosingToken[A <: EnhancedSyntaxToken] extends EnhancedSyntaxToken {
  def children: Seq[SyntaxToken]

  def withChildren(newChildren: Seq[SyntaxToken]): A
}

case class BraceExpressionToken(children: Seq[SyntaxToken]) extends EnclosingToken[BraceExpressionToken] {
  override def withChildren(newChildren: Seq[SyntaxToken]): BraceExpressionToken = BraceExpressionToken(newChildren)
}

case class ParenthesisExpressionToken(children: Seq[SyntaxToken]) extends EnclosingToken[ParenthesisExpressionToken] {
  override def withChildren(newChildren: Seq[SyntaxToken]): ParenthesisExpressionToken = ParenthesisExpressionToken(newChildren)
}

case class FunctionToken(parameters: ParenthesisExpressionToken, body: BraceExpressionToken) extends SyntaxToken