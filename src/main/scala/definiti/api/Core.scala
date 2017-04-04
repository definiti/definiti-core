package definiti.api

import definiti._

object Core {
  val unit = NativeClassDefinition("unit", Seq(), Seq(), None)

  val any = NativeClassDefinition("any", Seq(), Seq(), None)

  def string: ClassDefinition = find("String")

  def date: ClassDefinition = find("Date")

  def boolean: ClassDefinition = find("Boolean")

  def number: ClassDefinition = find("Number")

  private def find(name: String): ClassDefinition = {
    TypeReference.findType(name) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"An attempt to get $name definition was made with no result")
    }
  }
}