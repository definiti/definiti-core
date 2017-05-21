package definiti.core

package object parser {
  type ImportsMap = Map[String, String]

  val emptyImportsMap: ImportsMap = Map.empty[String, String]

  val NOT_DEFINED: String = "NOT_DEFINED"

  val BOOLEAN = "Boolean"
}
