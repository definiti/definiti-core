package state

sealed trait Syntax

case object EndOfLine extends Syntax
case object Space extends Syntax
case object OpenBrace extends Syntax
case object CloseBrace extends Syntax
case object OpenParenthesis extends Syntax
case object CloseParenthesis extends Syntax
case object Colon extends Syntax
case object Dot extends Syntax
case object Underscore extends Syntax
case class Symbol(content: String) extends Syntax
case class QuotedString(content: String) extends Syntax
case class Word(content: String) extends Syntax
case object VerificationKeyword extends Syntax
case object VerifyKeyword extends Syntax
case object VerifyingKeyword extends Syntax
case object TypeKeyword extends Syntax

case object Void extends Syntax
case object Unknown extends Syntax