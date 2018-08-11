package definiti.core

import definiti.common.ast.TypeReference

object Constants {
  val boolean = TypeReference("Boolean")
  val integer = TypeReference("Integer")
  val number = TypeReference("Number")
  val string = TypeReference("String")
  val unit = TypeReference("unit")
  val date = TypeReference("Date")

  def list(typ: String) = TypeReference("List", Seq(TypeReference(typ)))

  def option(typ: String) = TypeReference("Option", Seq(TypeReference(typ)))
}
