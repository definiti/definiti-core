package definiti.core.end2end

import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.ast._

class NamedFunctionSpec extends EndToEndSpec {
  import NamedFunctionSpec._

  "Project.generatePublicAST" should "generate the AST for the valid named function 'contains'" in {
    val expected = Ok(validContains)
    val output = processFile("namedFunction.contains")
    output should beResult[Root](expected)
  }

  "Project.generatePublicAST" should "give error for the invalid named function 'contains' when generics are invalid" in {
    val expected = Ko[Root](invalidContainsGenerics)
    val output = processFile("namedFunction.invalid-contains-generics")
    output should beResult[Root](expected)
  }

  "Project.generatePublicAST" should "accept generics in named functions" in {
    val output = processFile("namedFunction.nonEmptyList")
    output shouldBe ok[Root]
  }
}

object NamedFunctionSpec {
  val validContainsSrc = "src/test/resources/samples/namedFunction/contains.def"
  val validContains = Root(Seq(
    NamedFunction(
      name = "contains",
      fullName = "contains",
      genericTypes = Seq("A"),
      parameters = Seq(
        ParameterDefinition(
          name = "list",
          typeReference = TypeReference("List", Seq(TypeReference("A"))),
          location = Location(validContainsSrc, 1, 17, 1, 30)
        ),
        ParameterDefinition(
          name = "element",
          typeReference = TypeReference("A"),
          location = Location(validContainsSrc, 1, 32, 1, 42)
        )
      ),
      returnType = TypeReference("Boolean"),
      body = MethodCall(
        expression = Reference(
          name = "list",
          returnType = TypeReference("List", Seq(TypeReference("A"))),
          location = Location(validContainsSrc, 2, 3, 2, 7)
        ),
        method = "exists",
        parameters = Seq(
          LambdaExpression(
            parameterList = Seq(ParameterDefinition(
              name = "item",
              typeReference = TypeReference("A"),
              location = Location(validContainsSrc, 2, 16, 2, 23)
            )),
            expression = LogicalExpression(
              operator = LogicalOperator.Equal,
              left = Reference(
                name = "item",
                returnType = TypeReference("A"),
                location = Location(validContainsSrc, 2, 29, 2, 33)
              ),
              right = Reference(
                name = "element",
                returnType = TypeReference("A"),
                location = Location(validContainsSrc, 2, 37, 2, 44)
              ),
              returnType = TypeReference("Boolean"),
              location = Location(validContainsSrc, 2, 29, 2, 44)
            ),
            returnType = TypeReference("Boolean"),
            location = Location(validContainsSrc, 2, 15, 2, 45)
          )
        ),
        generics = Seq.empty,
        returnType = TypeReference("Boolean"),
        location = Location(validContainsSrc, 2, 3, 2, 46)
      ),
      location = Location(validContainsSrc, 1, 1, 3, 2)
    )
  ))

  val invalidContainsGenericsSrc = "src/test/resources/samples/namedFunction/invalid-contains-generics.def"
  val invalidContainsGenerics = Seq(AlertLocation(
    message = "Class B not found when trying to determine the type of the expression",
    location = Location(invalidContainsGenericsSrc, 2, 3, 2, 7)
  ))
}