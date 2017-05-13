package definiti.core.generators

import org.scalacheck.Gen

object Generators {
  def listOfBoundedSize[A](min: Int, max: Int, gen: Gen[A]): Gen[List[A]] = for {
    numElems <- Gen.choose(min, max)
    elems <- Gen.listOfN(numElems, gen)
  } yield elems
}
