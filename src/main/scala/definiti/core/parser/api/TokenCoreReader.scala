package definiti.core.parser.api

import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenCoreReader(tokens: Seq[TokenCore]) extends Reader[TokenCore] {
  override def first: TokenCore = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

  override def rest: Reader[TokenCore] = new TokenCoreReader(tokens.tail)
}
