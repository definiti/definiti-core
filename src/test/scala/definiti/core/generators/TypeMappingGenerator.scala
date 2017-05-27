package definiti.core.generators

import definiti.core.linking.TypeMapping
import org.scalacheck.Gen

object TypeMappingGenerator {
  lazy val anyTypeMapping: Gen[TypeMapping] = for {
    numberOfEntries <- Gen.posNum[Int]
    keys <- Gen.listOfN(numberOfEntries, ASTGenerator.anyIdentifier)
    values <- Gen.listOfN(numberOfEntries, ASTGenerator.anyDottedIdentifier)
  } yield {
    Map(keys.zip(values): _*)
  }
}
