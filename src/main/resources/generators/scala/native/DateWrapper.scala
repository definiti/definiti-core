import java.util.Date

// TODO: Use newer Date API
class DateWrapper(private val inner: Date) {
  def timestamp: NumberWrapper = new NumberWrapper(inner.getTime())

  def day: NumberWrapper = new NumberWrapper(inner.getDate)

  def month: NumberWrapper = new NumberWrapper(inner.getMonth)

  def ==(dateWrapper: DateWrapper): BooleanWrapper = timestamp == dateWrapper.timestamp

  def !=(dateWrapper: DateWrapper): BooleanWrapper = timestamp != dateWrapper.timestamp

  def >(dateWrapper: DateWrapper): BooleanWrapper = timestamp > dateWrapper.timestamp

  def <(dateWrapper: DateWrapper): BooleanWrapper = timestamp < dateWrapper.timestamp

  def >=(dateWrapper: DateWrapper): BooleanWrapper = timestamp >= dateWrapper.timestamp

  def <=(dateWrapper: DateWrapper): BooleanWrapper = timestamp <= dateWrapper.timestamp
}