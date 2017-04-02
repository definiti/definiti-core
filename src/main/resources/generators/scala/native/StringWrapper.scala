class StringWrapper(private val inner: String) {
  def nonEmpty(): BooleanWrapper = inner.nonEmpty

  def trim(): StringWrapper = new StringWrapper(inner.trim)

  def startsWith(prefix: StringWrapper): BooleanWrapper = inner.startsWith(prefix.inner)

  def matches(regex: StringWrapper): BooleanWrapper = inner.matches(regex.inner)
}