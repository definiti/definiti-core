package object booleanWrapperUtils {
  implicit def booleanToBooleanWrapper(boolean: Boolean): BooleanWrapper = new BooleanWrapper(boolean)
  implicit def booleanWrapperToBoolean(booleanWrapper: BooleanWrapper): Boolean = booleanWrapper.inner
}

import booleanWrapperUtils._

class BooleanWrapper(val inner: Boolean) {

}