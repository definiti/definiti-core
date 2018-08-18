package definiti.core.parser.project

import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenProjectReader(tokens: Seq[TokenProject]) extends Reader[TokenProject] {
  override def first: TokenProject = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

  override def rest: Reader[TokenProject] = new TokenProjectReader(tokens.tail)
}
