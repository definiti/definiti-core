import java.util.Date

class ListWrapper[A](private val inner: List[A]) {
  def nonEmpty(): Boolean = inner.nonEmpty

  def isEmpty(): Boolean = inner.isEmpty

  def head: A = inner.head

  def forall(f: A => Boolean): Boolean = inner.forall(f)

  def exists(f: A => Boolean): Boolean = inner.exists(f)

  def foldLeft[B](startValue: B, f: (B, A) => B): B = inner.foldLeft(startValue)(f)
}