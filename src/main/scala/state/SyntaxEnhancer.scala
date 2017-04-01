package state

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SyntaxEnhancer {
  type Syntax = Seq[SyntaxToken]
  
  def enhanceSyntax(source: Syntax): Syntax = {
    ???
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

  private[state] def removeEOLFromParenthesis(source: Syntax): Syntax = {
    // Hypothesis: We suppose the tree small enough to support recursion
    def process(sourceProcess: Syntax): Syntax = {
      sourceProcess.foldLeft(List[SyntaxToken]()) { (acc, token) =>
        token match {
          case token: ParenthesisExpressionToken => acc :+ token.mapOnContainers(processParenthesis)
          case token: ContainerToken[_] => acc :+ token.mapOnContainers(process)
          case _ => acc :+ token
        }
      }
    }

    def processParenthesis(sourceParenthesis: Syntax): Syntax = {
      sourceParenthesis.foldLeft(List[SyntaxToken]()) { (acc, token) =>
        token match {
          case EndOfLine => acc
          case token: ParenthesisExpressionToken => acc :+ token.mapOnContainers(processParenthesis)
          case token: ContainerToken[_] => acc :+ token.mapOnContainers(process)
          case _ => acc :+ token
        }
      }
    }
    process(source)
  }

  private[state] def trimEOLFromBrace(source: Syntax): Syntax = {
    // Hypothesis: We suppose the tree small enough to support recursion
    def process(sourceProcess: Syntax): Syntax = {
      sourceProcess.foldLeft(List[SyntaxToken]()) { (acc, token) =>
        token match {
          case token: BraceExpressionToken => acc :+ token.mapOnContainers(trimEOL).mapOnContainers(process)
          case token: ContainerToken[_] => acc :+ token.mapOnContainers(process)
          case _ => acc :+ token
        }
      }
    }

    process(source)
  }

  private[state] def ignoreEOLInEncloser(source: Syntax): Syntax = {
    trimEOLFromBrace(removeEOLFromParenthesis(trimEOL(source)))
  }

  private[state] def trimEOL(source: Syntax): Syntax = {
    source.dropWhile(_ == EndOfLine).reverse.dropWhile(_ == EndOfLine).reverse
  }

  private[state] def buildFirstClassCitizen(source: Syntax): Syntax = {
    @tailrec
    def process(acc: Syntax, remaining: Syntax): Syntax = remaining match {
      case Nil => acc
      case VerificationKeyword :: Word(verificationName) :: (body: BraceExpressionToken) :: tail =>
        process(acc :+ VerificationToken(verificationName, body), tail)
      case TypeKeyword :: Word(typeName) :: tail =>
        processType(typeName, tail) match {
          case (typeToken, remainingTail) => process(acc :+ typeToken, remainingTail)
        }
      case (token@(LineComment(_) | BlockComment(_))) :: tail =>
        process(acc :+ token, tail)
      case token =>
        throw new RuntimeException("Unexpected token: " + token)
    }

    def processType(typeName: String, remaining: Syntax): (TypeToken, Syntax) = remaining match {
      case AssignSymbol :: Word(aliasName) :: tail =>
        processAliasType(typeName, aliasName, Nil, tail)
      case (VerifyingKeyword | BraceExpressionToken(_)) :: tail => processDefinedType(typeName, Nil, remaining)
        processDefinedType(typeName, Nil, remaining)
      case token =>
        throw new RuntimeException("Unexpected token: " + token)
    }

    @tailrec
    def processAliasType(typeName: String, aliasName: String, verifyingReferences: Seq[String], remaining: Syntax): (TypeToken, Syntax) = remaining match {
      case VerifyingKeyword :: Word(verifyingReference) :: tail =>
        processAliasType(typeName, aliasName, verifyingReferences :+ verifyingReference, tail)
      case tail =>
        (TypeToken(typeName, Left(aliasName), verifyingReferences), tail)
    }

    @tailrec
    def processDefinedType(typeName: String, verifyingReferences: Seq[String], remaining: Syntax): (TypeToken, Syntax) = remaining match {
      case VerifyingKeyword :: Word(verifyingReference) :: tail =>
        processDefinedType(typeName, verifyingReferences :+ verifyingReference, tail)
      case (body: BraceExpressionToken) :: tail =>
        (TypeToken(typeName, Right(body), verifyingReferences), tail)
      case token =>
        throw new RuntimeException("Unexpected token: " + token)
    }

    process(Nil, source.filter(_ != EndOfLine))
  }

  private[state] def buildFunctions(source: Syntax): Syntax = {
    @tailrec
    def process(acc: Syntax, source: Syntax): Syntax = source match {
      case Nil =>
        acc
      case (parameters: ParenthesisExpressionToken) :: MapSymbol :: (body: BraceExpressionToken) :: tail =>
        process(acc :+ FunctionToken(parameters, body.mapOnContainers(buildFunctions)), tail)
      case (head: ContainerToken[_]) :: tail =>
        process(acc :+ head.mapOnContainers(buildFunctions), tail)
      case head :: tail =>
        process(acc :+ head, tail)
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
      case Nil => (ConditionToken(condition.mapOnContainers(buildConditions), bodyIf.mapOnContainers(buildConditions), None), Nil)
      case EndOfLine :: tail => processElse(condition, bodyIf, skippedEOL + 1, tail)
      case ElseKeyword :: tail => processElseBody(condition, bodyIf, tail)
      case tail => (ConditionToken(condition.mapOnContainers(buildConditions), bodyIf.mapOnContainers(buildConditions), None), Seq.fill(skippedEOL)(EndOfLine) ++ tail)
    }
    @tailrec
    def processElseBody(condition: ParenthesisExpressionToken, trueBody: BraceExpressionToken, remaining: Syntax): (ConditionToken, Syntax) = remaining match {
      case Nil => throw new RuntimeException("Unexpected end of block")
      case EndOfLine :: tail => processElseBody(condition, trueBody, tail)
      case (bodyElse: BraceExpressionToken) :: tail => (ConditionToken(condition.mapOnContainers(buildConditions), trueBody.mapOnContainers(buildConditions), Some(bodyElse.mapOnContainers(buildConditions))), tail)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }

    process(Nil, source)
  }

  private[state] def buildMethodOrAttributeCall(syntax: Syntax): Syntax = {
    @tailrec
    def process(acc: Syntax, source: Syntax): Syntax = source match {
      case Nil =>
        acc
      case (word @ Word(variableName)) :: tail =>
        processDot(variableName, tail) match {
          case Some((enhancedSyntax: EnhancedSyntaxToken, remaining)) => process(acc :+ enhancedSyntax, remaining)
          case None => process(acc :+ word, tail)
        }
      case (container: ContainerToken[_]) :: tail =>
        process(acc :+ container.mapOnContainers(buildMethodOrAttributeCall), tail)
      case token :: tail =>
        process(acc :+ token, tail)
    }
    @tailrec
    def processDot(variableName: String, source: Syntax): Option[(EnhancedSyntaxToken, Syntax)] = source match {
      case Nil => None
      case EndOfLine :: tail => processDot(variableName, tail)
      case Dot :: tail => processMethodOrAttributeName(variableName, tail)
      case _ => None
    }
    @tailrec
    def processMethodOrAttributeName(variableName: String, source: Syntax): Option[(EnhancedSyntaxToken, Syntax)] = source match {
      case Nil => None
      case EndOfLine :: tail => processMethodOrAttributeName(variableName, tail)
      case Word(methodName) :: tail => processParenthesis(variableName, methodName, tail)
      case _ => None
    }
    def processParenthesis(variableName: String, methodOrAttributeName: String, source: Syntax): Option[(EnhancedSyntaxToken, Syntax)] = source match {
      case Nil => Some(AttributeCall(variableName, methodOrAttributeName), Nil)
      case (parenthesis: ParenthesisExpressionToken) :: tail => Some(MethodCall(variableName, methodOrAttributeName, parenthesis), tail)
      case tail => Some(AttributeCall(variableName, methodOrAttributeName), tail)
    }
    process(Nil, syntax)
  }
}
