package definiti

package object core {
  private[core] type ImportsMap = Map[String, String]

  private[core] val emptyImportsMap: ImportsMap = Map.empty[String, String]

  private[core] val NOT_DEFINED: String = "NOT_DEFINED"

  private[core] val BOOLEAN = "Boolean"
}
