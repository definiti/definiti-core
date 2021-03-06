package definiti.common.ast

case class Location(
  file: String,
  range: Range
) {
  def prettyPrint: String = {
    s"$file:${range.prettyPrint}"
  }
}

object Location {
  def apply(file: String, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Location = {
    new Location(file, Range(startLine, startColumn, endLine, endColumn))
  }

  def apply(file: String, start: Position, end: Position): Location = {
    new Location(file, Range(start, end))
  }
}

case class Range(
  start: Position,
  end: Position
) {
  def prettyPrint: String = {
    s"${start.prettyPrint}-${end.prettyPrint}"
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
    s"$line:$column"
  }
}

object Position {
  val default = Position(0, 0)
}