package definiti.core.typing

import definiti.core.ast.pure.{DefinedFunction => PureDefinedFunction}
import definiti.core.ast.typed.{DefinedFunction => TypedDefinedFunction}
import definiti.core.{Context, DefinedFunctionContext, Validated}

class FunctionTyping(context: Context) {
  def addTypesIntoDefinedFunction(definedFunction: PureDefinedFunction): Validated[TypedDefinedFunction] = {
    val functionContext = DefinedFunctionContext(
      outerContext = context,
      currentFunction = definedFunction
    )
    val expressionTyping = new ExpressionTyping(functionContext)
    val validatedExpression = expressionTyping.addTypesIntoExpression(definedFunction.body)
    validatedExpression.map { expression =>
      TypedDefinedFunction(
        parameters = definedFunction.parameters,
        body = expression,
        genericTypes = definedFunction.genericTypes,
        range = definedFunction.range
      )
    }
  }
}
