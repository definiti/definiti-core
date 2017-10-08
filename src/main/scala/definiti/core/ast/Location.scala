package definiti.core.ast

case class Location(
  file: String,
  range: Range
) {
  def prettyPrint: String = {
    s"$file:$range"
  }
}

case class Range(
  start: Position,
  end: Position
) {
  def prettyPrint: String = {
    s"[${start.prettyPrint}, ${end.prettyPrint}]"
  }
}

object Range {
  def apply(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Range = {
    new Range(Position(startLine, startColumn), Position(endLine, endColumn))
  }

  val default = Range(Position.default, Position.default)
}

case class Position(
  line: Int,
  column: Int
) {
  def prettyPrint: String = {
    s"$line-$column"
  }
}

object Position {
  val default = Position(0, 0)
}