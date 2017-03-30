package state

sealed trait AST

case class TopLevel(
  verifications: Seq[Verification],
  types: Seq[Type]
) extends AST

case class Verification(name: String, message: String, check: Function) extends AST

case class Type(name: String, attributes: Seq[Attribute], verifications: Seq[String]) extends AST

case class Attribute(name: String, typeReference: String) extends AST

case class Variable(name: String, typeReference: String) extends AST

case class Parameter(name: String, typeReference: String) extends AST

sealed trait Scope {
  def variables: Seq[Variable]
}

case class SimpleScope(variables: Seq[Variable]) extends Scope

sealed trait Expression {
  def returnType(scope: Scope): String
}

case class BlockScope(
  variables: Seq[Variable],
  expressions: Seq[Expression]
) extends Expression with Scope {
  override def returnType(scope: Scope): String = expressions.lastOption.map(_.returnType(scope)).getOrElse("Void")
}

case class CallMethod(expression: Expression, name: String, parameters: Seq[Expression]) extends Expression {
  // TODO: here: check the type of the method from the expression
  override def returnType(scope: Scope): String = ???
}

case class VariableExpression(variable: String) extends Expression {
  override def returnType(scope: Scope): String = scope.variables.find(_.name == variable).map(_.typeReference).getOrElse("Void")
}

case class CallAttribute(expression: Expression, attribute: String) extends Expression {
  // TODO: here: check the type of the attribute from the expression
  override def returnType(scope: Scope): String = ???
}

case class If(
  condition: Expression,
  whenTrue: Expression,
  whenFalse: Expression
) extends Expression {
  override def returnType(scope: Scope): String = {
    if (whenTrue.returnType(scope) == whenFalse.returnType(scope)) {
      whenTrue.returnType(scope)
    } else {
      "Any"
    }
  }
}

case object EmptyExpression extends Expression {
  override def returnType(scope: Scope): String = "Void"
}

sealed trait Function extends Expression {
  def name: String

  def parameters: Seq[Parameter]
}

case class NativeFunction(name: String, parameters: Seq[Parameter]) extends Function {
  override def returnType(scope: Scope): String = "TODO"
}

case class DefinedFunction(name: String, parameters: Seq[Parameter], expression: Expression) extends Function {
  override def returnType(scope: Scope): String = {
    val variablesFromParameters = parameters.map(p => Variable(p.name, p.typeReference))
    val newVariables = variablesFromParameters ++ scope.variables.filterNot(v => variablesFromParameters.exists(_.name == v.name))
    expression.returnType(SimpleScope(newVariables))
  }
}

sealed trait LogicalExpression extends Expression {
  override def returnType(scope: Scope): String = "Boolean"
}

case class NumberValueExpression(number: BigDecimal) extends Expression {
  override def returnType(scope: Scope): String = "Number"
}

case class StringValueExpression(string: String) extends Expression {
  override def returnType(scope: Scope): String = "String"
}

case object True extends Expression {
  override def returnType(scope: Scope): String = "Boolean"
}

case object False extends Expression {
  override def returnType(scope: Scope): String = "Boolean"
}

case class Or(left: Expression, right: Expression) extends LogicalExpression
case class And(left: Expression, right: Expression) extends LogicalExpression
case class Upper(left: Expression, right: Expression) extends LogicalExpression
case class Lower(left: Expression, right: Expression) extends LogicalExpression
case class Equals(left: Expression, right: Expression) extends LogicalExpression
case class Plus(left: Expression, right: Expression) extends LogicalExpression
case class Minus(left: Expression, right: Expression) extends LogicalExpression
case class Time(left: Expression, right: Expression) extends LogicalExpression
case class Divide(left: Expression, right: Expression) extends LogicalExpression