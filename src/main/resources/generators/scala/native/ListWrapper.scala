import java.util.Date

class ListWrapper[A](private val inner: List[A]) {
  def nonEmpty(): Boolean = inner.nonEmpty

  def isEmpty(): Boolean = inner.isEmpty

  def head: A = inner.head
}