package definiti.core.utils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

private[core] object CollectionUtils {
  def scalaSeq[A](list: java.util.List[A]): Seq[A] = {
    list.asScala.toList
  }

  def scalaSeq[A](stream: java.util.stream.Stream[A]): Seq[A] = {
    val buffer = ListBuffer[A]()
    stream.forEach((a) => buffer.append(a))
    buffer
  }
}
