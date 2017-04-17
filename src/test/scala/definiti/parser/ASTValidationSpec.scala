package definiti.parser

import definiti._
import definiti.api.ReferenceContext
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
  val coreClasses = Seq(booleanDefinition, numberDefinition, stringDefinition, dateDefinition, listDefinition)
  val trueExpression = BooleanValue(value = true, noRange)

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of boolean expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(trueExpression) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of And expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(And(trueExpression, trueExpression, noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of Equal expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(Equal(trueExpression, trueExpression, noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of Number expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(NumberValue(1, noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of String expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(QuotedStringValue("", noRange)) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate the type of condition expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      Condition(
        condition = trueExpression,
        onTrue = trueExpression,
        onFalse = None,
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate combined expressions" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
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
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      Variable(
        name = "myVariable",
        typeReference = TypeReference("Boolean", Seq()),
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate attribute call expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = Variable("myDate", TypeReference("Date", Seq()), noRange),
        attribute = "timestamp",
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate method call expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      MethodCall(
        expression = Variable("myString", TypeReference("String", Seq()), noRange),
        method = "nonEmpty",
        Seq(),
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate attribute call expression with generic type" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = Variable("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange),
        attribute = "head",
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate method call expression with generic type" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      MethodCall(
        expression = Variable("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange),
        method = "nonEmpty",
        parameters = Seq(),
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate chained attribute call expression with generic type" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = AttributeCall(
          expression = Variable("myList", TypeReference("List", Seq(TypeReference("Date", Seq()))), noRange),
          attribute = "head",
          range = noRange
        ),
        attribute = "timestamp",
        range = noRange
      )
    ) should ===(Valid)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate chained method call expression with generic type" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    ASTValidation.validateTypeReferenceOfExpression(
      AttributeCall(
        expression = MethodCall(
          expression = Variable("myList", TypeReference("List", Seq(TypeReference("Date", Seq()))), noRange),
          method = "randomElement",
          parameters = Seq(),
          range = noRange
        ),
        attribute = "timestamp",
        range = noRange
      )
    ) should ===(Valid)
  }
}
