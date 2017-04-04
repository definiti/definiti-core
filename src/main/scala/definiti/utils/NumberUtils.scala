package definiti.utils

object NumberUtils {
  def isNumberExpression(str: String): Boolean = {
    str.forall(c => c.isDigit || c == '.') && str.count(_ == '.') <= 1
  }
}
