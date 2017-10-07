package definiti.core.utils

private[core] object StringUtils {
  def lastPart(source: String, separator: Char): String = {
    if (source.last == separator) {
      lastPart(source.substring(0, source.length - 1), separator)
    } else if (source.contains(separator)) {
      source.substring(source.lastIndexOf(".") + 1)
    } else {
      source
    }
  }
}
