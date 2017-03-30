package state

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SyntaxEnhancer {
  def enhanceSyntax(source: Seq[Syntax]): Seq[Syntax] = {
    var enhancedSyntaxStack = List[OpeningSyntax]()
    var accStack = List[ListBuffer[Syntax]](ListBuffer())
    source.foreach {
      case open: OpeningSyntax =>
        enhancedSyntaxStack = open :: enhancedSyntaxStack
        accStack = ListBuffer[Syntax]() :: accStack
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

  private[state] def buildEnclosing(source: Seq[Syntax]): Seq[Syntax] = {
    var enhancedSyntaxStack = List[OpeningSyntax]()
    var accStack = List[ListBuffer[Syntax]](ListBuffer())
    source.foreach {
      case open: OpeningSyntax =>
        enhancedSyntaxStack = open :: enhancedSyntaxStack
        accStack = ListBuffer[Syntax]() :: accStack
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

  private[state] def trimEOLAroundEncloser(source: Seq[Syntax]): Seq[Syntax] = {
    @tailrec def flatProcess(acc: Seq[Syntax], source: Seq[Syntax]): Seq[Syntax] = source match {
      case Nil => acc
      case EndOfLine :: (encloser: EnclosingSyntax[_]) :: tail => flatProcess(acc :+ encloser, tail)
      case head :: tail => flatProcess(acc :+ head, tail)
    }

    // We suppose the imbrication of encloser is not enough to make a stack overflow
    def process(source: Seq[Syntax]): Seq[Syntax] = {
      val flatTrimmedEOL = flatProcess(Nil, trimEOL(source))
      flatTrimmedEOL.map {
        case enclosingSyntax: EnclosingSyntax[_] => enclosingSyntax.withChildren(process(enclosingSyntax.children))
        case other => other
      }
    }

    process(source)
  }

  private[state] def trimEOL(source: Seq[Syntax]): Seq[Syntax] = {
    source.dropWhile(_ == EndOfLine).reverse.dropWhile(_ == EndOfLine).reverse
  }

  private[state] def buildFunctions(source: Seq[Syntax]): Seq[Syntax] = {
    @tailrec
    def process(acc: Seq[Syntax], source: Seq[Syntax]): Seq[Syntax] = source match {
      case Nil =>
        acc
      case (parameters: ParenthesisExpressionSyntax) :: Symbol("=>") :: (body: BraceExpressionSyntax) :: tail =>
        process(acc :+ FunctionSyntax(parameters, processChild(body)), tail)
      case (head: EnclosingSyntax[_]) :: tail =>
        process(acc :+ processChild(head), tail)
      case head :: tail =>
        process(acc :+ head, tail)
    }

    def processChild[A <: EnhancedSyntax](enclosingSyntax: EnclosingSyntax[A]): A = {
      enclosingSyntax.withChildren(process(Nil, enclosingSyntax.children))
    }

    process(Nil, source)
  }
}
