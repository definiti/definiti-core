class NumberWrapper(private val inner: BigDecimal) {
  def -(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner - numberWrapper.inner)

  def +(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner + numberWrapper.inner)

  def *(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner * numberWrapper.inner)

  def /(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner / numberWrapper.inner)

  def %(numberWrapper: NumberWrapper): NumberWrapper = new NumberWrapper(inner % numberWrapper.inner)

  def ==(numberWrapper: NumberWrapper): BooleanWrapper = inner == numberWrapper.inner

  def !=(numberWrapper: NumberWrapper): BooleanWrapper = inner != numberWrapper.inner

  def >(numberWrapper: NumberWrapper): BooleanWrapper = inner > numberWrapper.inner

  def <(numberWrapper: NumberWrapper): BooleanWrapper = inner < numberWrapper.inner

  def >=(numberWrapper: NumberWrapper): BooleanWrapper = inner >= numberWrapper.inner

  def <=(numberWrapper: NumberWrapper): BooleanWrapper = inner <= numberWrapper.inner
}