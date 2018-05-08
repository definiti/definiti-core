package definiti.common.utils

object StringUtils {
  def lastPart(source: String, separator: Char = '.'): String = {
    if (source.isEmpty) {
      source
    } else if (source.last == separator) {
      lastPart(source.substring(0, source.length - 1), separator)
    } else if (source.contains(separator)) {
      source.substring(source.lastIndexOf(".") + 1)
    } else {
      source
    }
  }

  def canonical(prefix: String, suffix: String, separator: Char = '.'): String = {
    if (prefix.nonEmpty) {
      prefix + "." + suffix
    } else {
      suffix
    }
  }
}
