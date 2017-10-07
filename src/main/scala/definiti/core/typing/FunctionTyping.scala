package definiti.core.typing

import definiti.core.ast.DefinedFunction
import definiti.core.ast.pure.PureDefinedFunction
import definiti.core.{Context, DefinedFunctionContext, Validated}

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
        range = definedFunction.range
      )
    }
  }
}
