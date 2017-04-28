package definiti.core.utils

import scala.collection.JavaConverters._

object CollectionUtils {
  def scalaSeq[A](list: java.util.List[A]): Seq[A] = {
    list.asScala.toList
  }
}
