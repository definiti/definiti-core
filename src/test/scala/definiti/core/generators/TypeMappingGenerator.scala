package definiti.core.generators

import definiti.core.Context
import definiti.core.parser.ProjectLinking.TypeMapping
import org.scalacheck.Gen

object TypeMappingGenerator {
  def anyTypeMapping(implicit context: Context): Gen[TypeMapping] = for {
    numberOfEntries <- Gen.posNum[Int]
    keys <- Gen.listOfN(numberOfEntries, ASTGenerator.anyIdentifier)
    values <- Gen.listOfN(numberOfEntries, ASTGenerator.anyDottedIdentifier)
  } yield {
    Map(keys.zip(values): _*)
  }
}
