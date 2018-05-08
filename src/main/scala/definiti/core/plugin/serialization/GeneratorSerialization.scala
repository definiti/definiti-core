package definiti.core.plugin.serialization

import java.nio.file.{Path, Paths}

import spray.json._

private[core] trait GeneratorSerialization {
  self: JsonSerialization =>

  def filesFromJson(json: String): Map[Path, String] = {
    val jsValue = json.parseJson
    jsValue match {
      case JsObject(fields) =>
        fields.map { case (path, contentJsValue) =>
          contentJsValue match {
            case JsString(content) => createPath(path) -> content
            case _ => deserializationError(s"Expected string, got: ${contentJsValue}")
          }
        }
      case _ => deserializationError(s"Expected object, got: ${jsValue}")
    }
  }

  private def createPath(str: String): Path = {
    val path = Paths.get(str)
    if (!path.isAbsolute && !str.startsWith(".")) {
      Paths.get(".", str)
    } else {
      path
    }
  }
}
