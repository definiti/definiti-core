package definiti.core.end2end

import definiti.core.Ok
import definiti.core.ProgramResultMatchers._
import definiti.core.ast._

class VerificationFunctionSpec extends EndToEndSpec {
  import VerificationFunctionSpec._

  "Project.generatePublicAST" should "generate the AST with verification calling functions in same file" in {
    val expected = Ok(validVerificationFunction)
    val output = processFile("verificationFunction.verificationFunction")
    output should beResult[Root](expected)
  }
}

object VerificationFunctionSpec {
  import EndToEndSpec._

  val verificationFunctionSrc = "src/test/resources/samples/verificationFunction/verificationFunction.def"
  val verificationFunctionLocation = LocationPath(verificationFunctionSrc)
  val validVerificationFunction = Root(Seq(
    Namespace(
      name = "verificationFunction",
      fullName = "verificationFunction",
      elements = Seq(
        Verification(
          name = "IsStrongPassword",
          fullName = "verificationFunction.IsStrongPassword",
          message = LiteralMessage("The password is too simple", verificationFunctionLocation(4, 3, 31)),
          function = DefinedFunction(
            parameters = Seq(ParameterDefinition(
              name = "string",
              typeReference = TypeReference("String"),
              location = verificationFunctionLocation(5, 4, 5, 18)
            )),
            body = LogicalExpression(
              operator = LogicalOperator.UpperOrEqual,
              left = FunctionCall(
                name = "verificationFunction.passwordScore",
                parameters = Seq(Reference(
                  name = "string",
                  returnType = TypeReference("String"),
                  location = verificationFunctionLocation(6, 19, 25)
                )),
                generics = Seq.empty,
                returnType = TypeReference("Number"),
                location = verificationFunctionLocation(6, 5, 26)
              ),
              right = NumberValue(30, TypeReference("Number"), verificationFunctionLocation(6, 30, 32)),
              returnType = TypeReference("Boolean"),
              location = verificationFunctionLocation(6, 5, 32)
            ),
            genericTypes = Seq.empty,
            location = verificationFunctionLocation(5, 3, 7, 4)
          ),
          comment = None,
          location = verificationFunctionLocation(3, 1, 8, 2)
        ),
        NamedFunction(
          name = "passwordScore",
          fullName = "verificationFunction.passwordScore",
          genericTypes = Seq.empty,
          parameters = Seq(ParameterDefinition("string", TypeReference("String"), verificationFunctionLocation(10, 19, 33))),
          returnType = TypeReference("Number"),
          body = CalculatorExpression(
            operator = CalculatorOperator.Plus,
            left = FunctionCall(
              name = "verificationFunction.scoreSpecialCharacter",
              parameters = Seq(Reference("string", TypeReference("String"), verificationFunctionLocation(11, 25, 31))),
              generics = Seq.empty,
              returnType = TypeReference("Number"),
              location = verificationFunctionLocation(11, 3, 32)
            ),
            right = AttributeCall(
              expression = Reference("string", TypeReference("String"), verificationFunctionLocation(11, 35, 41)),
              attribute = "length",
              returnType = TypeReference("Number"),
              location = verificationFunctionLocation(11, 35, 48)
            ),
            returnType = TypeReference("Number"),
            location = verificationFunctionLocation(11, 3, 48)
          ),
          location = verificationFunctionLocation(10, 1, 12, 2)
        ),
        NamedFunction(
          name = "scoreSpecialCharacter",
          fullName = "verificationFunction.scoreSpecialCharacter",
          genericTypes = Seq.empty,
          parameters = Seq(ParameterDefinition("string", TypeReference("String"), verificationFunctionLocation(14, 27, 41))),
          returnType = TypeReference("Number"),
          body = Condition(
            condition = MethodCall(
              expression = Reference("string", TypeReference("String"), verificationFunctionLocation(15, 7, 13)),
              method = "matches",
              parameters = Seq(QuotedStringValue(".*[^a-zA-Z0-9].*", TypeReference("String"), verificationFunctionLocation(15, 22, 40))),
              generics = Seq.empty,
              returnType = TypeReference("Boolean"),
              location = verificationFunctionLocation(15, 7, 41)
            ),
            onTrue = NumberValue(20, TypeReference("Number"), verificationFunctionLocation(16, 5, 7)),
            onFalse = Some(NumberValue(0, TypeReference("Number"), verificationFunctionLocation(18, 5, 6))),
            returnType = TypeReference("Number"),
            location = verificationFunctionLocation(15, 3, 19, 4)
          ),
          location = verificationFunctionLocation(14, 1, 20, 2)
        )
      )
    )
  ))
}