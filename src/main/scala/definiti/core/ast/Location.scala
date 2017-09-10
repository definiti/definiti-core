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

case class Position(
  line: Int,
  column: Int
) {
  def prettyPrint: String = {
    s"$line-$column"
  }
}