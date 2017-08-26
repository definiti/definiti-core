package definiti.core.validation

import definiti.core._
import org.scalatest.{FlatSpec, Matchers}

class ASTValidationSpec extends FlatSpec with Matchers {
  val noRange = Range(Position(0, 0), Position(0, 0))
  val booleanDefinition = NativeClassDefinition("Boolean", Seq(), Seq(), Seq(), None)
  val numberDefinition = NativeClassDefinition("Number", Seq(), Seq(), Seq(), None)
  val stringDefinition = NativeClassDefinition(
    "String",
    Seq(),
    Seq(),
    Seq(NativeMethodDefinition("nonEmpty", Seq(), Seq(), TypeReference("Boolean", Seq()), None)),
    None
  )
  val dateDefinition = NativeClassDefinition(
    "Date",
    Seq(),
    Seq(AttributeDefinition("timestamp", TypeReference("Number", Seq()), None, Seq(), noRange)),
    Seq(),
    None
  )
  val listDefinition = NativeClassDefinition(
    "List",
    Seq("A"),
    Seq(AttributeDefinition("head", TypeReference("A", Seq()), None, Seq(), noRange)),
    Seq(
      NativeMethodDefinition("nonEmpty", Seq(), Seq(), TypeReference("Boolean", Seq()), None),
      NativeMethodDefinition("randomElement", Seq(), Seq(), TypeReference("A", Seq()), None)
    ),
    None
  )
  val unitDefinition = NativeClassDefinition("Unit", Seq.empty, Seq.empty, Seq.empty, None)
  val coreClasses = Seq(booleanDefinition, numberDefinition, stringDefinition, dateDefinition, listDefinition, unitDefinition)
  val trueExpression = BooleanValue(value = true, noRange)

  private val configuration = ConfigurationMock()
  private val astValidation = new ASTValidation(configuration)

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of boolean expression" in {
    implicit val context = baseReferenceContext
    astValidation.validateTypeReferenceOfExpression(trueExpression) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of And expression" in {
    implicit val context = baseReferenceContext
    astValidation.validateTypeReferenceOfExpression(And(trueExpression, trueExpression, noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of Equal expression" in {
    implicit val context = baseReferenceContext
    astValidation.validateTypeReferenceOfExpression(Equal(trueExpression, trueExpression, noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of Number expression" in {
    implicit val context = baseReferenceContext
    astValidation.validateTypeReferenceOfExpression(NumberValue(1, noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of String expression" in {
    implicit val context = baseReferenceContext
    astValidation.validateTypeReferenceOfExpression(QuotedStringValue("", noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of condition expression" in {
    implicit val context = baseReferenceContext
    astValidation.validateTypeReferenceOfExpression(
      Condition(
        condition = trueExpression,
        onTrue = trueExpression,
        onFalse = None,
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate combined expressions" in {
    implicit val context = baseReferenceContext
    astValidation.validateTypeReferenceOfExpression(
      CombinedExpression(
        Seq(
          Condition(
            condition = trueExpression,
            onTrue = trueExpression,
            onFalse = None,
            range = noRange
          ),
          trueExpression
        ),
        noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate variable expression" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myVariable", TypeReference("Boolean", Seq()), noRange)
    )
    astValidation.validateTypeReferenceOfExpression(
      Reference(
        name = "myVariable",
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate attribute call expression" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myDate", TypeReference("Date", Seq()), noRange)
    )
    astValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = Reference("myDate", noRange),
        attribute = "timestamp",
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate method call expression" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myString", TypeReference("String", Seq()), noRange)
    )
    astValidation.validateTypeReferenceOfExpression(
      MethodCall(
        expression = Reference("myString", noRange),
        method = "nonEmpty",
        Seq(),
        Seq(),
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate attribute call expression with generic type" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange)
    )
    astValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = Reference("myList", noRange),
        attribute = "head",
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate method call expression with generic type" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange)
    )
    astValidation.validateTypeReferenceOfExpression(
      MethodCall(
        expression = Reference("myList", noRange),
        method = "nonEmpty",
        parameters = Seq(),
        generics = Seq(),
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate chained attribute call expression with generic type" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myList", TypeReference("List", Seq(TypeReference("Date", Seq()))), noRange)
    )
    astValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = AttributeCall(
          expression = Reference("myList", noRange),
          attribute = "head",
          range = noRange
        ),
        attribute = "timestamp",
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate chained method call expression with generic type" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myList", TypeReference("List", Seq(TypeReference("Date", Seq()))), noRange)
    )
    astValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = MethodCall(
          expression = Reference("myList", noRange),
          method = "randomElement",
          parameters = Seq(),
          generics = Seq(),
          range = noRange
        ),
        attribute = "timestamp",
        range = noRange
      )
    ) should ===(Valid)
  }

  private lazy val baseReferenceContext = ReferenceContext(
    classes = coreClasses,
    verifications = Seq.empty,
    namedFunctions = Seq.empty,
    requirements = Seq.empty
  )

  private def baseMethodContext(parameters: ParameterDefinition*) = MethodContext(
    outerContext = baseReferenceContext,
    currentMethod = baseMethodDefinition(parameters),
    genericTypes = Seq.empty
  )

  private def baseMethodDefinition(parameters: Seq[ParameterDefinition] = Seq.empty) = DefinedMethodDefinition(
    name = "myMethod",
    genericTypes = Seq.empty,
    function = DefinedFunction(
      parameters = parameters,
      body = BooleanValue(value = true, range = noRange),
      genericTypes = Seq.empty,
      range = noRange
    ),
    comment = None,
    range = noRange
  )
}
