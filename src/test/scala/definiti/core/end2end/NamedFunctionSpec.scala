package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.{Ko, Ok}
import definiti.common.validation.AlertLocation
import definiti.core.ProgramResultMatchers._
import definiti.common.utils.ASTUtils._

class NamedFunctionSpec extends EndToEndSpec {
  import NamedFunctionSpec._

  "Project.generatePublicAST" should "generate the AST for the valid named function 'contains'" in {
    val expected = Ok(validContains)
    val output = processFile("namedFunction.contains")
    output should beResult[Root](expected)
  }

  it should "give error for the invalid named function 'contains' when generics are invalid" in {
    val expected = Ko[Root](invalidContainsGenerics)
    val output = processFile("namedFunction.invalid-contains-generics")
    output should beResult[Root](expected)
  }

  it should "accept generics in named functions" in {
    val output = processFile("namedFunction.nonEmptyList")
    output shouldBe ok[Root]
  }

  it should "define implicitly a type by the parameter name" in {
    val expected = Ok(implicitType)
    val output = processFile("namedFunction.implicit-type")
    output should beResult[Root](expected)
  }

  it should "refuse a type by its parameter name when the type does not exist" in {
    val output = processFile("namedFunction.invalid-implicit-type")
    output shouldBe ko[Root]
  }

  it should "define implicitly a type by the lambda parameter name" in {
    val expected = Ok(implicitLambdaParameterType)
    val output = processFile("namedFunction.implicit-lambda-parameter-type")
    output should beResult[Root](expected)
  }

  it should "refuse a type by its parameter name when the type does not exist in lambda" in {
    val output = processFile("namedFunction.invalid-implicit-lambda-parameter-type")
    output shouldBe ko[Root]
  }
}

object NamedFunctionSpec {
  val validContainsSrc = "src/test/resources/samples/namedFunction/contains.def"
  val validContains = root(
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
  )

  val invalidContainsGenericsSrc = "src/test/resources/samples/namedFunction/invalid-contains-generics.def"
  val invalidContainsGenerics = Seq(AlertLocation(
    message = "Class B not found when trying to determine the type of the expression",
    location = Location(invalidContainsGenericsSrc, 2, 3, 2, 7)
  ))

  val implicitTypeSrc = "src/test/resources/samples/namedFunction/implicit-type.def"
  val implicitType = root(
    NamedFunction(
      name = "nonEmpty",
      fullName = "nonEmpty",
      genericTypes = Seq.empty,
      parameters = Seq(
        ParameterDefinition(
          name = "string",
          typeReference = TypeReference("String"),
          location = Location(implicitTypeSrc, 1, 14, 1, 20)
        )
      ),
      returnType = TypeReference("Boolean"),
      body = MethodCall(
        expression = Reference(
          name = "string",
          returnType = TypeReference("String"),
          location = Location(implicitTypeSrc, 2, 3, 2, 9)
        ),
        method = "nonEmpty",
        parameters = Seq.empty,
        generics = Seq.empty,
        returnType = TypeReference("Boolean"),
        location = Location(implicitTypeSrc, 2, 3, 2, 20)
      ),
      location = Location(implicitTypeSrc, 1, 1, 3, 2)
    )
  )

  val implicitLambdaParameterTypeSrc = "src/test/resources/samples/namedFunction/implicit-lambda-parameter-type.def"
  val implicitLambdaParameterType = root(
    NamedFunction(
      name = "contains",
      fullName = "contains",
      genericTypes = Seq("A"),
      parameters = Seq(
        ParameterDefinition(
          name = "list",
          typeReference = TypeReference("List", Seq(TypeReference("A"))),
          location = Location(implicitLambdaParameterTypeSrc, 1, 17, 1, 30)
        ),
        ParameterDefinition(
          name = "element",
          typeReference = TypeReference("A"),
          location = Location(implicitLambdaParameterTypeSrc, 1, 32, 1, 42)
        )
      ),
      returnType = TypeReference("Boolean"),
      body = MethodCall(
        expression = Reference(
          name = "list",
          returnType = TypeReference("List", Seq(TypeReference("A"))),
          location = Location(implicitLambdaParameterTypeSrc, 2, 3, 2, 7)
        ),
        method = "exists",
        parameters = Seq(
          LambdaExpression(
            parameterList = Seq(ParameterDefinition(
              name = "a",
              typeReference = TypeReference("A"),
              location = Location(implicitLambdaParameterTypeSrc, 2, 16, 2, 17)
            )),
            expression = LogicalExpression(
              operator = LogicalOperator.Equal,
              left = Reference(
                name = "a",
                returnType = TypeReference("A"),
                location = Location(implicitLambdaParameterTypeSrc, 2, 23, 2, 24)
              ),
              right = Reference(
                name = "element",
                returnType = TypeReference("A"),
                location = Location(implicitLambdaParameterTypeSrc, 2, 28, 2, 35)
              ),
              returnType = TypeReference("Boolean"),
              location = Location(implicitLambdaParameterTypeSrc, 2, 23, 2, 35)
            ),
            returnType = TypeReference("Boolean"),
            location = Location(implicitLambdaParameterTypeSrc, 2, 15, 2, 36)
          )
        ),
        generics = Seq.empty,
        returnType = TypeReference("Boolean"),
        location = Location(implicitLambdaParameterTypeSrc, 2, 3, 2, 37)
      ),
      location = Location(implicitLambdaParameterTypeSrc, 1, 1, 3, 2)
    )
  )
}