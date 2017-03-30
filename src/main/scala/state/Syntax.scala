package state

sealed trait Syntax

sealed trait OpeningSyntax

sealed trait ClosingSyntax {
  def opening: OpeningSyntax

  def enhancer(children: Seq[Syntax]): EnhancedSyntax
}

case object EndOfLine extends Syntax
case object Space extends Syntax

case object OpenBrace extends Syntax with OpeningSyntax

case object CloseBrace extends Syntax with ClosingSyntax {
  override def opening: OpeningSyntax = OpenBrace

  override def enhancer(children: Seq[Syntax]): EnhancedSyntax = BraceExpressionSyntax(children)
}

case object OpenParenthesis extends Syntax with OpeningSyntax

case object CloseParenthesis extends Syntax with ClosingSyntax {
  override def opening: OpeningSyntax = OpenParenthesis

  override def enhancer(children: Seq[Syntax]): EnhancedSyntax = ParenthesisExpressionSyntax(children)
}
case object Colon extends Syntax
case object Dot extends Syntax
case object Underscore extends Syntax

case object Comma extends Syntax
case class Symbol(content: String) extends Syntax
case class QuotedString(content: String) extends Syntax
case class Word(content: String) extends Syntax
case object VerificationKeyword extends Syntax
case object VerifyKeyword extends Syntax
case object VerifyingKeyword extends Syntax
case object TypeKeyword extends Syntax

case object Void extends Syntax
case object Unknown extends Syntax

sealed trait EnhancedSyntax extends Syntax

sealed trait EnclosingSyntax[A <: EnhancedSyntax] extends EnhancedSyntax {
  def children: Seq[Syntax]
  def withChildren(newChildren: Seq[Syntax]): A
}

case class BraceExpressionSyntax(children: Seq[Syntax]) extends EnclosingSyntax[BraceExpressionSyntax] {
  override def withChildren(newChildren: Seq[Syntax]): BraceExpressionSyntax = BraceExpressionSyntax(newChildren)
}

case class ParenthesisExpressionSyntax(children: Seq[Syntax]) extends EnclosingSyntax[ParenthesisExpressionSyntax] {
  override def withChildren(newChildren: Seq[Syntax]): ParenthesisExpressionSyntax = ParenthesisExpressionSyntax(newChildren)
}

case class FunctionSyntax(parameters: ParenthesisExpressionSyntax, body: BraceExpressionSyntax) extends Syntax