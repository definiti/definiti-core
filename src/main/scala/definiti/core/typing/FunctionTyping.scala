package definiti.core.typing

import definiti.common.ast._
import definiti.common.validation.Validated
import definiti.core.ast.pure.PureDefinedFunction
import definiti.core.{Context, DefinedFunctionContext}

private[core] class FunctionTyping(context: Context) {
  def addTypesIntoDefinedFunction(definedFunction: PureDefinedFunction): Validated[DefinedFunction] = {
    val functionContext = DefinedFunctionContext(
      outerContext = context,
      currentFunction = definedFunction
    )
    val expressionTyping = new ExpressionTyping(functionContext)
    val validatedExpression = expressionTyping.addTypesIntoExpression(definedFunction.body)
    validatedExpression.map { expression =>
      DefinedFunction(
        parameters = definedFunction.parameters,
        body = expression,
        genericTypes = definedFunction.genericTypes,
        location = definedFunction.location
      )
    }
  }
}
