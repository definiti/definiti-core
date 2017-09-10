package definiti.core

package object ast {
  private[core] type ImportsMap = Map[String, String]

  private[core] val emptyImportsMap: ImportsMap = Map.empty[String, String]
}
