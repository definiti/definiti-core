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
}

case class Position(
  line: Int,
  column: Int
) {
  def prettyPrint: String = {
    s"$line-$column"
  }
}