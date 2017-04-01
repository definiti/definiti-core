package state

import jdk.internal.org.objectweb.asm.TypeReference

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

case class LineComment(content: String) extends SyntaxToken

case class BlockComment(content: String) extends SyntaxToken

case class Symbol(content: String) extends SyntaxToken

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

case object AssignSymbol extends SyntaxToken

case object EqualSymbol extends SyntaxToken

case object NotSymbol extends SyntaxToken

case object NotEqualSymbol extends SyntaxToken

case object LowerSymbol extends SyntaxToken

case object UpperSymbol extends SyntaxToken

case object LowerOrEqualSymbol extends SyntaxToken

case object UpperOrEqualSymbol extends SyntaxToken

case object MapSymbol extends SyntaxToken

case object AndSymbol extends SyntaxToken

case object OrSymbol extends SyntaxToken

case object PlusSymbol extends SyntaxToken

case object MinusSymbol extends SyntaxToken

case object TimeSymbol extends SyntaxToken

case object DivideSymbol extends SyntaxToken

case object ModuloSymbol extends SyntaxToken

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

case class FunctionToken(parameters: ParenthesisExpressionToken, body: BraceExpressionToken) extends ContainerToken[FunctionToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): FunctionToken = {
    FunctionToken(
      parameters,
      BraceExpressionToken(map(body.children))
    )
  }
}

case class ConditionToken(condition: ParenthesisExpressionToken, onTrue: BraceExpressionToken, onFalse: Option[BraceExpressionToken]) extends ContainerToken[ConditionToken] {
  override def mapOnContainers(map: (Seq[SyntaxToken]) => Seq[SyntaxToken]): ConditionToken = {
    ConditionToken(
      ParenthesisExpressionToken(map(condition.children)),
      BraceExpressionToken(map(onTrue.children)),
      onFalse.map(body => BraceExpressionToken(map(body.children)))
    )
  }
}

case class MethodCall(variable: String, method: String, parameters: ParenthesisExpressionToken) extends EnhancedSyntaxToken

case class AttributeCall(variable: String, attribute: String) extends EnhancedSyntaxToken

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