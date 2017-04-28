package definiti.core.utils

import definiti.core._

private[core] object Core {
  val unit = NativeClassDefinition("unit", Seq(), Seq(), Seq(), None)

  val any = NativeClassDefinition("any", Seq(), Seq(), Seq(), None)

  def string(implicit context: Context): ClassDefinition = find("String")

  def date(implicit context: Context): ClassDefinition = find("Date")

  def boolean(implicit context: Context): ClassDefinition = find("Boolean")

  def number(implicit context: Context): ClassDefinition = find("Number")

  private def find(name: String)(implicit context: Context): ClassDefinition = {
    context.findType(name) match {
      case Some(classDefinition) => classDefinition
      case None => throw new RuntimeException(s"An attempt to get $name definition was made with no result")
    }
  }
}