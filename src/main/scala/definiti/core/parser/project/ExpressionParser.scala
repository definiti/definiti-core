package definiti.core.parser.project

import definiti.common.ast
import definiti.common.ast._

trait ExpressionParser {
  self: DefinitiFileParser =>

  /*
    For expression, we need to consider left-recursion expressions.
    It means that for an expression like `5 < value && value < 10`
    If we define the parser like:
    expr :=
      | expr && expr
      | expr < expr
      | [0-9]+
      | [a-z]+

    The parser will be confused and will not be able to resolve the expression correctly.
    The result could be a failure or something like `lower(5, and(value, lower(value, 10)))` that is invalid.
    So avoid this kind of thing, we must define the order of expressions:

    expr := logical
    logical := order && order | order
    order := atomic < atomic | atomic
    atomic := [0-9]+ | [a-z]+

    It brings complexity for the reading but is a necessity for the parser to work.
    Also, the above examples were simplified to help understanding but the code is actually a little more complex.
    Please feel free to update it and launch tests to understand the impact.
   */

  lazy val expression: PackratParser[Expression] = {
    combinationExpression
  }

  lazy val combinationExpression: PackratParser[Expression] = {
    (logicalExpression ~ (logicalCombinationOperator ~ logicalExpression).+) ^^ {
      case left ~ stack =>
        stack.foldLeft(left) { case (acc, operator ~ right) =>
          LogicalExpression(
            operator = operator.kind,
            left = acc,
            right = right,
            returnType = unset,
            location = location(Range(
              left.location.range.start,
              right.location.range.end
            ))
          )
        }
    } | logicalExpression
  }

  lazy val logicalExpression: PackratParser[Expression] = {
    (calculatorExpressionLevel2 ~ (logicalOperator ~ calculatorExpressionLevel2).+) ^^ {
      case left ~ stack =>
        stack.foldLeft(left) { case (acc, operator ~ right) =>
          LogicalExpression(
            operator = operator.kind,
            left = acc,
            right = right,
            returnType = unset,
            location = location(Range(
              left.location.range.start,
              right.location.range.end
            ))
          )
        }
    } | calculatorExpressionLevel2
  }

  lazy val calculatorExpressionLevel2: PackratParser[Expression] = {
    (calculatorExpressionLevel1 ~ (calculatorOperatorLevel2 ~ calculatorExpressionLevel1).+) ^^ {
      case left ~ stack =>
        stack.foldLeft(left) { case (acc, operator ~ right) =>
          CalculatorExpression(
            operator = operator.kind,
            left = acc,
            right = right,
            returnType = unset,
            location = location(Range(
              left.location.range.start,
              right.location.range.end
            ))
          )
        }
    } | calculatorExpressionLevel1
  }

  lazy val calculatorExpressionLevel1: PackratParser[Expression] = {
    (otherExpression ~ (calculatorOperatorLevel1 ~ otherExpression).+) ^^ {
      case left ~ stack =>
        stack.foldLeft(left) { case (acc, operator ~ right) =>
          CalculatorExpression(
            operator = operator.kind,
            left = acc,
            right = right,
            returnType = unset,
            location = location(Range(
              left.location.range.start,
              right.location.range.end
            ))
          )
        }
    } | otherExpression
  }

  lazy val otherExpression: PackratParser[Expression] = {
    (
      lambdaExpression
        | container(`(`, expression, `)`)
        | okExpression
        | koExpression
        | methodExpression
        | attributeExpression
        | functionExpression
        | notExpression
        | atomicExpression
        | conditionExpression
      )
  }

  lazy val lambdaExpression: PackratParser[LambdaExpression] = {
    (
      parameterListDefinition ~
        `=>` ~ rangedContainer(`{`, expression, `}`)
      ) ^^ {
      case parameters ~ _ ~ innerExpression =>
        LambdaExpression(
          parameterList = parameters.value,
          expression = innerExpression.value,
          returnType = unset,
          location = location(range(parameters, innerExpression))
        )
    }
  }

  lazy val okExpression: PackratParser[OkValue] = {
    `ok` ^^ { token =>
      OkValue(unset, location(token))
    }
  }

  lazy val koExpression: PackratParser[KoValue] = {
    (
      `ko` ~ rangedContainer(`(`, joinedElements(expression, `,`), `)`).?
      ) ^^ {
      case firstToken ~ parameters =>
        KoValue(
          parameters = parameters.map(_.value).getOrElse(Seq.empty),
          returnType = unset,
          location = location(Range(
            start = position(firstToken.pos),
            end = parameters
              .map(_.range.end)
              .getOrElse(position(firstToken.pos))
          ))
        )
    }

  }

  lazy val methodExpression: PackratParser[MethodCall] = {
    (
      expression ~ `.` ~ identifier ~
        container(`[`, genericTypeList, `]`).? ~
        rangedContainer(`(`, joinedElements(expression, `,`).?, `)`)
      ) ^^ {
      case inner ~ _ ~ methodName ~
        generics ~
        parameters =>
        MethodCall(
          expression = inner,
          method = methodName.value,
          parameters = parameters.value.getOrElse(Seq.empty),
          generics = generics.getOrElse(Seq.empty).map(_.value),
          returnType = unset,
          location = location(Range(
            start = inner.location.range.start,
            end = parameters.range.end
          ))
        )
    }
  }

  lazy val attributeExpression: PackratParser[AttributeCall] = {
    (
      expression ~ `.` ~ identifier
      ) ^^ {
      case inner ~ _ ~ attributeName =>
        ast.AttributeCall(
          expression = inner,
          attribute = attributeName.value,
          returnType = unset,
          location = location(Range(
            start = inner.location.range.start,
            end = position(attributeName.posEnd)
          ))
        )
    }
  }

  lazy val functionExpression: PackratParser[FunctionCall] = {
    (
      identifier ~
        container(`[`, genericTypeList, `]`).? ~
        rangedContainer(`(`, joinedElements(expression, `,`), `)`)
      ) ^^ {
      case functionName ~ generics ~ parameters =>
        ast.FunctionCall(
          name = functionName.value,
          parameters = parameters.value,
          generics = generics.getOrElse(Seq.empty).map(_.value),
          returnType = unset,
          location = location(range(functionName, parameters))
        )
    }
  }

  lazy val notExpression: PackratParser[Not] = {
    (
      not ~ expression
      ) ^^ {
      case firstToken ~ inner =>
        Not(
          inner = inner,
          returnType = unset,
          location = location(Range(
            start = position(firstToken.pos),
            end = inner.location.range.end
          ))
        )
    }
  }

  lazy val atomicExpression: PackratParser[AtomicExpression] = {
    (
      boolean ^^ { boolean => BooleanValue(boolean.value, unset, location(boolean)) }
        | number ^^ { number => NumberValue(number.value, unset, location(number)) }
        | integer ^^ { integer => IntegerValue(integer.value, unset, location(integer)) }
        | string ^^ { string => QuotedStringValue(string.value, unset, location(string)) }
        | identifier ^^ { reference => Reference(reference.value, unset, location(reference)) }
      )
  }

  lazy val conditionExpression: PackratParser[Condition] = {
    (
      `if` ~ container(`(`, expression, `)`) ~
        rangedContainer(`{`, combinedExpression, `}`) ~
        (
          `else` ~
            rangedContainer(`{`, combinedExpression, `}`)
          ).?
      ) ^^ {
      case firstToken ~ condition ~ ifBody ~ elseOpt =>
        Condition(
          condition = condition,
          onTrue = ifBody.value,
          onFalse = elseOpt.map { case _ ~ elseBody => elseBody.value },
          returnType = unset,
          location = location(range(
            firstToken,
            elseOpt
              .map { case _ ~ elseBody => elseBody }
              .getOrElse(ifBody)
          ))
        )
    }
  }
}
