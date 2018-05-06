package definiti

package object core {
  private[core] type ImportsMap = Map[String, String]

  private[core] val emptyImportsMap: ImportsMap = Map.empty[String, String]

  private[core] val BOOLEAN = "Boolean"
}
