package definiti.core.api

import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.utils.Core
import org.scalatest.{FlatSpec, Matchers}

class ASTHelperSpec extends FlatSpec with Matchers {
  val noRange = Range(Position(0, 0), Position(0, 0))
  val booleanDefinition = NativeClassDefinition("Boolean", Seq(), Seq(), Seq(), None)
  val numberDefinition = NativeClassDefinition("Number", Seq(), Seq(), Seq(), None)
  val stringDefinition = NativeClassDefinition(
    "String",
    Seq(),
    Seq(),
    Seq(MethodDefinition("nonEmpty", Seq(), Seq(), TypeReference("Boolean", Seq()), None)),
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
      MethodDefinition("nonEmpty", Seq(), Seq(), TypeReference("Boolean", Seq()), None),
      MethodDefinition("randomElement", Seq(), Seq(), TypeReference("A", Seq()), None)
    ),
    None
  )
  val coreClasses = Seq(booleanDefinition, numberDefinition, stringDefinition, dateDefinition, listDefinition)
  val trueExpression = BooleanValue(value = true, noRange)

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Boolean for a Boolean expression" in {
    implicit val context = baseReferenceContext
    val input = trueExpression
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Boolean for a And expression" in {
    implicit val context = baseReferenceContext
    val input = LogicalExpression(LogicalOperator.And, trueExpression, trueExpression, noRange)
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Boolean for a Equal expression" in {
    implicit val context = baseReferenceContext
    val input = LogicalExpression(LogicalOperator.Equal, trueExpression, trueExpression, noRange)
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a Number for a NumberValue expression" in {
    implicit val context = baseReferenceContext
    val input = NumberValue(1, noRange)
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a String for a QuotedString expression" in {
    implicit val context = baseReferenceContext
    val input = QuotedStringValue("", noRange)
    val expected = Some(ClassReference(stringDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return a number expression on condition with onTrue and onFalse as Number" in {
    implicit val context = baseReferenceContext
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
    implicit val context = baseReferenceContext
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
    implicit val context = baseReferenceContext
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
    implicit val context = baseReferenceContext
    val input = CombinedExpression(Seq(
      trueExpression,
      NumberValue(1, noRange)
    ), noRange)
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type of the variable in variable expression" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myVariable", TypeReference("Number", Seq()), noRange)
    )
    val input = Reference("myVariable", noRange)
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type of the attribute in attribute call expression" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myDate", TypeReference("Date", Seq()), noRange)
    )
    val input = AttributeCall(
      expression = Reference("myDate", noRange),
        attribute = "timestamp",
        range = noRange
    )
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type of the method return type in method call expression" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myString", TypeReference("String", Seq()), noRange)
    )
    val input = MethodCall(
      expression = Reference("myString", noRange),
      method = "nonEmpty",
      Seq(),
      Seq(),
      range = noRange
    )
    val expected = Some(ClassReference(booleanDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type the variable with its generic defined" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange)
    )
    val input = Reference("myList", noRange)
    val expected = Some(ClassReference(listDefinition, Seq(ClassReference(numberDefinition, Seq()))))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.getReturnTypeOptOfExpression" should "return the type the attribute from generic type" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange)
    )
    val input = AttributeCall(
      expression = Reference("myList", noRange),
      attribute = "head",
      range = noRange
    )
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  "ASTValidation.validateTypeReferenceOfExpression" should "validate method call expression from generic type" in {
    implicit val context = baseMethodContext(
      ParameterDefinition("myList", TypeReference("List", Seq(TypeReference("Number", Seq()))), noRange)
    )
    val input = MethodCall(
      expression = Reference("myList", noRange),
      method = "randomElement",
      parameters = Seq(),
      generics = Seq(),
      range = noRange
    )
    val expected = Some(ClassReference(numberDefinition, Seq()))
    ASTHelper.getReturnTypeOptOfExpression(input) should ===(expected)
  }

  private lazy val baseReferenceContext = ReferenceContext(
    classes = coreClasses,
    verifications = Seq.empty,
    namedFunctions = Seq.empty
  )

  private def baseMethodContext(parameters: ParameterDefinition*) = MethodContext(
    outerContext = baseReferenceContext,
    currentMethod = baseMethodDefinition(parameters),
    genericTypes = Seq.empty
  )

  private def baseMethodDefinition(parameters: Seq[ParameterDefinition] = Seq.empty) = MethodDefinition(
    name = "myMethod",
    genericTypes = Seq.empty,
    parameters = parameters,
    returnType = TypeReference("Any", Seq.empty),
    comment = None
  )
}
