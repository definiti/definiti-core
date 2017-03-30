package state

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SyntaxEnhancer {
  type Syntax = Seq[SyntaxToken]
  
  def enhanceSyntax(source: Syntax): Syntax = {
    ???
  }

  private[state] def squashEOL(source: Syntax): Syntax = {
    source.foldLeft(List[SyntaxToken]()) { case (acc, elt) =>
      if (elt == EndOfLine && acc.lastOption.contains(EndOfLine)) {
        acc
      } else {
        acc :+ elt
      }
    }
  }

  private[state] def buildEnclosing(source: Syntax): Syntax = {
    var enhancedSyntaxStack = List[OpeningSyntax]()
    var accStack = List[ListBuffer[SyntaxToken]](ListBuffer())
    source.foreach {
      case open: OpeningSyntax =>
        enhancedSyntaxStack = open :: enhancedSyntaxStack
        accStack = ListBuffer[SyntaxToken]() :: accStack
      case close: ClosingSyntax =>
        enhancedSyntaxStack.headOption match {
          case Some(openingSyntax) if close.opening == openingSyntax =>
            val enhancedSyntax = close.enhancer(accStack.head)
            accStack = accStack.tail
            accStack.head.append(enhancedSyntax)
            enhancedSyntaxStack = enhancedSyntaxStack.tail
          case None =>
            throw new RuntimeException("Unexpected token: " + close)
        }
      case other => accStack.head.append(other)
    }

    enhancedSyntaxStack match {
      case Nil => accStack.head
      case open :: _ => throw new RuntimeException("Unclosed block: " + open)
    }
  }

  private[state] def trimEOLAroundEncloser(source: Syntax): Syntax = {
    @tailrec def flatProcess(acc: Syntax, source: Syntax): Syntax = source match {
      case Nil => acc
      case EndOfLine :: (encloser: EnclosingToken[_]) :: tail => flatProcess(acc :+ encloser, tail)
      case head :: tail => flatProcess(acc :+ head, tail)
    }

    // We suppose the imbrication of encloser is not enough to make a stack overflow
    def process(source: Syntax): Syntax = {
      val flatTrimmedEOL = flatProcess(Nil, trimEOL(source))
      flatTrimmedEOL.map {
        case enclosingSyntax: EnclosingToken[_] => enclosingSyntax.withChildren(process(enclosingSyntax.children))
        case other => other
      }
    }

    process(source)
  }

  private[state] def trimEOL(source: Syntax): Syntax = {
    source.dropWhile(_ == EndOfLine).reverse.dropWhile(_ == EndOfLine).reverse
  }

  private[state] def buildFunctions(source: Syntax): Syntax = {
    @tailrec
    def process(acc: Syntax, source: Syntax): Syntax = source match {
      case Nil =>
        acc
      case (parameters: ParenthesisExpressionToken) :: Symbol("=>") :: (body: BraceExpressionToken) :: tail =>
        process(acc :+ FunctionToken(parameters, processChild(body)), tail)
      case (head: EnclosingToken[_]) :: tail =>
        process(acc :+ processChild(head), tail)
      case head :: tail =>
        process(acc :+ head, tail)
    }

    def processChild[A <: EnhancedSyntaxToken](enclosingSyntax: EnclosingToken[A]): A = {
      enclosingSyntax.withChildren(process(Nil, enclosingSyntax.children))
    }

    process(Nil, source)
  }

  private[state] def extractParameterDefinition(source: ParenthesisExpressionToken): Seq[Parameter] = {
    @tailrec
    def process(acc: Seq[Parameter], remainingSyntax: Syntax): Seq[Parameter] = remainingSyntax match {
      case Nil => acc
      case Word(name) :: Colon :: Word(typeReference) :: tail => process(acc :+ Parameter(name, typeReference), tail)
      case Comma :: tail => process(acc, tail)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }
    process(Nil, source.children.filter(_ != EndOfLine))
  }

  private[state] def buildConditions(source: Syntax): Syntax = {
    @tailrec
    def process(acc: Syntax, remaining: Syntax): Syntax = remaining match {
      case Nil => acc
      case IfKeyword :: tail =>
        val (condition, remainingTail) = processCondition(tail)
        process(acc :+ condition, remainingTail)
      case (container: ContainerToken[_]) :: tail => process(acc :+ container.mapOnContainers(buildConditions), tail)
      case token :: tail => process(acc :+ token, tail)
    }
    @tailrec
    def processCondition(remaining: Syntax): (ConditionToken, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Unexpected end of block")
      case EndOfLine :: tail => processCondition(tail)
      case (condition: ParenthesisExpressionToken) :: tail => processIfBody(condition, tail)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }
    @tailrec
    def processIfBody(condition: ParenthesisExpressionToken, remaining: Syntax): (ConditionToken, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Unexpected end of block")
      case EndOfLine :: tail => processIfBody(condition, tail)
      case (bodyIf: BraceExpressionToken) :: tail => processElse(condition, bodyIf, 0, tail)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }
    @tailrec
    def processElse(condition: ParenthesisExpressionToken, bodyIf: BraceExpressionToken, skippedEOL: Int, remaining: Syntax): (ConditionToken, Syntax) = remaining match {
      case Nil => (ConditionToken(condition, bodyIf, None), Nil)
      case EndOfLine :: tail => processElse(condition, bodyIf, skippedEOL + 1, tail)
      case ElseKeyword :: tail => processElseBody(condition, bodyIf, tail)
      case tail => (ConditionToken(condition, bodyIf, None), Seq.fill(skippedEOL)(EndOfLine) ++ tail)
    }
    @tailrec
    def processElseBody(condition: ParenthesisExpressionToken, trueBody: BraceExpressionToken, remaining: Syntax): (ConditionToken, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Unexpected end of block")
      case EndOfLine :: tail => processElseBody(condition, trueBody, tail)
      case (bodyElse: BraceExpressionToken) :: tail => (ConditionToken(condition, trueBody, Some(bodyElse)), tail)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }

    process(Nil, source)
  }
}
