package definiti.core.plugin.serialization


import definiti.core.ast.Library
import spray.json.{JsArray, JsObject, JsString}

trait LibrarySerialization {
  self: JsonSerialization =>

  def libraryToJson(library: Library): String = {
    JsObject(
      field("packages", library.namespacesMap),
      field("verifications", library.verificationsMap),
      field("types", library.typesMap),
      field("namedFunctions", library.namedFunctionsMap)
    ).compactPrint
  }

  private def field(name: String, elements: Map[String, _]): (String, JsArray) = {
    name -> JsArray(elements.keys.map(JsString(_)).toVector)
  }
}
