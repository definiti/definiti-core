package definiti.common.utils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object CollectionUtils {
  def scalaSeq[A](list: java.util.List[A]): Seq[A] = {
    if (list != null) {
      list.asScala.toList
    } else {
      Seq.empty
    }
  }

  def scalaSeq[A](set: java.util.Set[A]): Seq[A] = {
    if (set != null) {
      set.asScala.toList
    } else {
      Seq.empty
    }
  }

  def scalaSeq[A](stream: java.util.stream.Stream[A]): Seq[A] = {
    if (stream != null) {
      val buffer = ListBuffer[A]()
      stream.forEach((a) => buffer.append(a))
      buffer
    } else {
      Seq.empty
    }
  }

  def javaList[A](seq: Seq[A]): java.util.List[A] = {
    new java.util.ArrayList[A](seq.asJava)
  }

  def squash[A, B](maps: Seq[Map[A, B]]): Map[A, B] = {
    maps.foldLeft(Map.empty[A, B])(_ ++ _)
  }
}
