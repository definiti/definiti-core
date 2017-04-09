package definiti.parser

import definiti.SyntaxEnhancer.trimEOL
import definiti._
import definiti.utils.NumberUtils

import scala.annotation.tailrec

object ExpressionParser {
  type Syntax = Seq[SyntaxToken]

  def buildExpression(syntax: Syntax): ExpressionToken = {
    def buildFinalExpression(syntax: Syntax): ExpressionToken = {
      syntax match {
        case NotSymbol :: tail =>
          NotExpression(buildFinalExpression(tail))
        case TrueKeyword :: Nil =>
          BooleanExpressionToken(true)
        case FalseKeyword :: Nil =>
          BooleanExpressionToken(false)
        case QuotedString(content) :: Nil =>
          QuotedStringExpressionToken(content)
        case Word(content) :: Nil if NumberUtils.isNumberExpression(content) =>
          NumberExpressionToken(BigDecimal(content))
        case Word(content) :: Nil =>
          VariableExpressionToken(content)
        case ParenthesisExpressionToken(tokens) :: Nil =>
          process(createSyntaxTreeWithSymbols(tokens))
        case (Word(_) | TrueKeyword | FalseKeyword | QuotedString(_) | ParenthesisExpressionToken(_)) :: Dot :: _ =>
          chainedCalls(syntax)
        case (condition: ConditionToken) :: Nil =>
          ConditionExpressionToken(
            condition = buildExpression(condition.condition.children),
            onTrue = CombinedExpressionToken(splitExpressionsFromBrace(condition.onTrue).map(buildExpression)).simplify(),
            onFalse = condition.onFalse.map(body => CombinedExpressionToken(splitExpressionsFromBrace(body).map(buildExpression)).simplify())
          )
        case token =>
          throw new RuntimeException("Unexpected token: " + token)
      }
    }

    def process(syntaxTree: SyntaxTree): ExpressionToken = syntaxTree match {
      case SyntaxTreeBranch(symbol, left, right) =>
        symbol.toExpressionToken(process(left), process(right))
      case SyntaxTreeLeaf(innerSyntax) => buildFinalExpression(innerSyntax)
    }

    process(createSyntaxTreeWithSymbols(syntax))
  }

  def removeComments(syntax: Syntax): Syntax = {
    syntax
      .filter(token => !(token.isInstanceOf[LineComment] || token.isInstanceOf[BlockComment]))
      .foldLeft(List[SyntaxToken]()) { (acc, token) =>
        if (token == EndOfLine && acc.lastOption.contains(EndOfLine)) {
          acc
        } else {
          acc :+ token
        }
      }
  }

  def splitParenthesisByComma(parenthesis: ParenthesisExpressionToken): Seq[Syntax] = {
    @tailrec
    def process(resultAcc: Seq[Syntax], acc: Syntax, remaining: Syntax): Seq[Syntax] = remaining match {
      case Nil =>
        if (acc.isEmpty) {
          resultAcc
        } else {
          resultAcc :+ acc
        }
      case Comma :: tail =>
        process(resultAcc :+ acc, Nil, tail)
      case token :: tail =>
        process(resultAcc, acc :+ token, tail)
    }

    process(Nil, Nil, trimEOL(removeComments(parenthesis.children)))
  }

  def splitExpressions(syntax: Syntax): Seq[Syntax] = {
    @tailrec
    def process(resultAcc: Seq[Syntax], acc: Syntax, remaining: Syntax): Seq[Syntax] = remaining match {
      case Nil =>
        if (acc.isEmpty) {
          resultAcc
        } else {
          resultAcc :+ acc
        }
      case EndOfLine :: token :: EndOfLine :: tail if token.isInstanceOf[IgnoreEOLWithBrace] =>
        process(resultAcc, acc :+ token, tail)
      case EndOfLine :: token :: tail if token.isInstanceOf[IgnoreEOLWithBrace] =>
        process(resultAcc, acc :+ token, tail)
      case token :: EndOfLine :: tail if token.isInstanceOf[IgnoreEOLWithBrace] =>
        process(resultAcc, acc :+ token, tail)
      case EndOfLine :: tail =>
        process(resultAcc :+ acc, Nil, tail)
      case token :: tail =>
        process(resultAcc, acc :+ token, tail)
    }

    process(Nil, Nil, trimEOL(removeComments(syntax)))
  }

  def splitExpressionsFromBrace(brace: BraceExpressionToken): Seq[Syntax] = splitExpressions(brace.children)

  def chainedCalls(syntax: Syntax): ExpressionToken = {
    def process(expressionToken: ExpressionToken, remaining: Syntax): ExpressionToken = remaining match {
      case Nil => expressionToken
      case Dot :: Word(methodName) :: (parameters: ParenthesisExpressionToken) :: tail =>
        process(MethodCallToken(expressionToken, methodName, ListExpressionToken(splitParenthesisByComma(parameters).map(buildExpression))), tail)
      case Dot :: Word(attributeName) :: tail =>
        process(AttributeCallToken(expressionToken, attributeName), tail)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }

    syntax match {
      case TrueKeyword :: tail =>
        process(BooleanExpressionToken(true), tail)
      case FalseKeyword :: tail =>
        process(BooleanExpressionToken(false), tail)
      case QuotedString(content) :: tail =>
        process(QuotedStringExpressionToken(content), tail)
      case Word(content) :: tail if NumberUtils.isNumberExpression(content) =>
        process(NumberExpressionToken(BigDecimal(content)), tail)
      case Word(content) :: tail =>
        process(VariableExpressionToken(content), tail)
      case ParenthesisExpressionToken(content) :: tail =>
        process(buildExpression(content), tail)
    }
  }

  sealed trait SyntaxTree

  case class SyntaxTreeBranch(symbol: Symbol, left: SyntaxTree, right: SyntaxTree) extends SyntaxTree

  case class SyntaxTreeLeaf(syntax: Syntax) extends SyntaxTree

  def createSyntaxTreeWithSymbols(syntax: Syntax): SyntaxTree = {
    val symbolsOrderedDescPriority = Seq(
      OrSymbol, AndSymbol,
      EqualSymbol, NotEqualSymbol, LowerSymbol, UpperSymbol, LowerOrEqualSymbol, UpperOrEqualSymbol,
      PlusSymbol, MinusSymbol, ModuloSymbol, TimeSymbol, DivideSymbol
    )

    def processForSymbol(syntax: Syntax, remainingSymbols: Seq[Symbol]): SyntaxTree = {
      remainingSymbols match {
        case Nil => SyntaxTreeLeaf(syntax)
        case symbol :: tail =>
          if (syntax.contains(symbol)) {
            val (left, right) = syntax.splitAt(syntax.indexOf(symbol))
            SyntaxTreeBranch(symbol, processForSymbol(left, tail), processForSymbol(right.tail, remainingSymbols))
          } else {
            processForSymbol(syntax, tail)
          }
      }
    }

    processForSymbol(syntax, symbolsOrderedDescPriority)
  }
}
