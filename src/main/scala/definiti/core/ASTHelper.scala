package definiti.core

object ASTHelper {
  def canonical(packageName: String, name: String): String = {
    if (packageName.nonEmpty) {
      packageName + "." + name
    } else {
      name
    }
  }
}
