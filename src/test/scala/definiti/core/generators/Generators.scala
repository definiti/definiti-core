package definiti.core.generators

import org.scalacheck.Gen

object Generators {
  def listOfBoundedSize[A](min: Int, max: Int, gen: Gen[A]): Gen[List[A]] = for {
    numElems <- Gen.choose(min, max)
    elems <- Gen.listOfN(numElems, gen)
  } yield elems

  def numberAsString: Gen[String] = Gen.oneOf(integerAsString, floatAsString)

  def integerAsString: Gen[String] = Gen.nonEmptyListOf(Gen.numChar).map(_.mkString(""))

  def floatAsString: Gen[String] = for {
    integer <- integerAsString
    decimals <- integerAsString
  } yield {
    integer + "." + decimals
  }

  def decreasingFrequency(min: Int, max: Int): Gen[Int] = {
    val frequencies = for (index <- min to max) yield {
      index -> Gen.const(max - index)
    }
    Gen.frequency(frequencies: _*)
  }

  def listDecreasingFrequencySize[A](min: Int, max: Int, gen: Gen[A]): Gen[List[A]] = {
   decreasingFrequency(min, max).flatMap(numberOfElements => Gen.listOfN(numberOfElements, gen))
  }
}
