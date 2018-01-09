package definiti.core.plugin.serialization


import definiti.core.ast.Library
import spray.json.{JsArray, JsObject, JsString}

trait LibrarySerialization {
  self: JsonSerialization =>

  def libraryToJson(library: Library): String = {
    JsObject(
      field("packages", library.packages),
      field("verifications", library.verifications),
      field("types", library.types),
      field("attributes", library.attributes),
      field("methods", library.methods),
      field("namedFunctions", library.namedFunctions)
    ).compactPrint
  }

  private def field(name: String, elements: Map[String, _]): (String, JsArray) = {
    name -> JsArray(elements.keys.map(JsString(_)).toVector)
  }
}
