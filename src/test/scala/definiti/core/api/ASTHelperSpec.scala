package definiti.core.api

import definiti.core.parser.{ASTValidation, Valid}
import definiti.core.{And, AttributeCall, AttributeDefinition, BooleanValue, CombinedExpression, Condition, Equal, MethodCall, NativeClassDefinition, NativeMethodDefinition, NumberValue, Position, QuotedStringValue, Range, TypeReference, Variable}
import org.scalatest.{FlatSpec, Matchers}

class ASTHelperSpec extends FlatSpec with Matchers {
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

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Boolean for a Boolean expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = trueExpression
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Boolean for a And expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = And(trueExpression, trueExpression, noRange)
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Boolean for a Equal expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = Equal(trueExpression, trueExpression, noRange)
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Number for a NumberValue expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = NumberValue(1, noRange)
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a String for a QuotedString expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = QuotedStringValue("", noRange)
    val expected = Some(ClassReference(stringDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a number expression on condition with onTrue and onFalse as Number" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = Condition(
      condition = trueExpression,
      onTrue = NumberValue(1, noRange),
      onFalse = Some(NumberValue(2, noRange)),
      range = noRange
    )
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return any expression on condition with onTrue as Number and onFalse as String" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = Condition(
      condition = trueExpression,
      onTrue = NumberValue(1, noRange),
      onFalse = Some(QuotedStringValue("", noRange)),
      range = noRange
    )
    val expected = Some(ClassReference(Core.any, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return unit expression on condition when onFalse is None" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = Condition(
      condition = trueExpression,
      onTrue = NumberValue(1, noRange),
      onFalse = None,
      range = noRange
    )
    val expected = Some(ClassReference(Core.unit, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type of the last expression of a combined expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = CombinedExpression(Seq(
      trueExpression,
      NumberValue(1, noRange)
    ), noRange)
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type of the variable in variable expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = Variable("myVariable", TypeReference("Number", Seq()), noRange)
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type of the attribute in attribute call expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = AttributeCall(
        expression = Variable("myDate", TypeReference("Date", Seq()), noRange),
        attribute = "timestamp",
        range = noRange
    )
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type of the method return type in method call expression" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = MethodCall(
      expression = Variable("myString", TypeReference("String", Seq()), noRange),
      method = "nonEmpty",
      Seq(),
      Seq(),
      range = noRange
    )
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type the variable with its generic defined" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = Variable("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange)
    val expected = Some(ClassReference(listDefinition, Seq(ClassReference(numberDefinition, Seq()))))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type the attribute from generic type" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = AttributeCall(
      expression = Variable("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange),
      attribute = "head",
      range = noRange
    )
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate method call expression from generic type" in {
    implicit val context = ReferenceContext(
      classes = coreClasses,
      verifications = Seq()
    )
    val input = MethodCall(
      expression = Variable("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange),
      method = "randomElement",
      parameters = Seq(),
      generics = Seq(),
      range = noRange
    )
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }
}
