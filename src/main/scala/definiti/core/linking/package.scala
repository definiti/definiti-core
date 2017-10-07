package definiti.core

package object linking {
  private[core] type TypeMapping = Map[String, String]

  private[core] def emptyTypeMapping = Map.empty[String, String]
}
