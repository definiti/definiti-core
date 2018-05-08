package definiti.common.tests

import definiti.common.ast.Location
import definiti.common.control.Control

case class LocationPath(path: String) {
  def apply(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Location = {
    Location(path, startLine, startColumn, endLine, endColumn)
  }

  def apply(line: Int, startColumn: Int, endColumn: Int): Location = {
    Location(path, line, startColumn, line, endColumn)
  }
}

object LocationPath {
  def control(name: String, file: String): LocationPath = {
    LocationPath(s"src/test/resources/samples/controls/${name}/${file}.def")
  }

  def control(control: Control[_], file: String): LocationPath = {
    LocationPath.control(control.name, file)
  }
}