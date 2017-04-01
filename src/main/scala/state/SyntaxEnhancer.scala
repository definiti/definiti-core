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
        process(acc :+ FunctionToken(extractParameterDefinition(parameters), body.mapOnContainers(buildFunctions)), tail)
      case (head: ContainerToken[_]) :: tail =>
        process(acc :+ head.mapOnContainers(buildFunctions), tail)
      case head :: tail =>
        process(acc :+ head, tail)
    }

    process(Nil, source)
  }

  private[state] def extractParameterDefinition(source: ParenthesisExpressionToken): Seq[FunctionParameter] = {
    @tailrec
    def process(acc: Seq[FunctionParameter], remainingSyntax: Syntax): Seq[FunctionParameter] = remainingSyntax match {
      case Nil => acc
      case Word(name) :: Colon :: Word(typeReference) :: tail => process(acc :+ FunctionParameter(name, typeReference), tail)
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

  private[state] def completeFirstClassCitizenStructure(syntax: Syntax): Syntax = {
    syntax.map {
      case token: VerificationToken => completeVerificationToken(token)
      case token: TypeToken => completeTypeToken(token)
      case token => token
    }
  }

  private[state] def completeVerificationToken(verificationToken: VerificationToken): StructuredVerificationToken = {
    verificationToken.body.children match {
      case QuotedString(message) :: EndOfLine :: (function: FunctionToken) :: Nil =>
        StructuredVerificationToken(verificationToken.name, message, function)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }
  }

  private[state] def completeTypeToken(typeToken: TypeToken): StructuredTypeToken = {
    typeToken.definition match {
      case Left(_) => completeAliasTypeToken(typeToken)
      case Right(_) => completeDefinedTypeToken(typeToken)
    }
  }

  private[state] def completeDefinedTypeToken(typeToken: TypeToken): StructuredDefinedTypeToken = {
    def extractTypeVerificationToken(body: BraceExpressionToken): TypeVerificationToken = body.children match {
      case QuotedString(message) :: EndOfLine :: (function: FunctionToken) :: Nil =>
        TypeVerificationToken(message, function)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }
    @tailrec
    def process(fieldsAcc: List[TypeFieldToken], verificationsAcc: List[TypeVerificationToken], remaining: Syntax): StructuredDefinedTypeToken = remaining match {
      case Nil =>
        StructuredDefinedTypeToken(typeToken.name, fieldsAcc, verificationsAcc, typeToken.verifications)
      case EndOfLine :: tail =>
        process(fieldsAcc, verificationsAcc, tail)
      case Word(fieldName) :: Colon :: Word(typeReference) :: Nil =>
        process(fieldsAcc :+ TypeFieldToken(fieldName, typeReference), verificationsAcc, Nil)
      case Word(fieldName) :: Colon :: Word(typeReference) :: EndOfLine :: tail =>
        process(fieldsAcc :+ TypeFieldToken(fieldName, typeReference), verificationsAcc, tail)
      case VerifyKeyword :: (body: BraceExpressionToken) :: Nil =>
        process(fieldsAcc, verificationsAcc :+ extractTypeVerificationToken(body), Nil)
    }
    process(Nil, Nil, typeToken.definition.right.get.children)
  }

  private[state] def completeAliasTypeToken(typeToken: TypeToken): StructuredAliasTypeToken = {
    StructuredAliasTypeToken(typeToken.name, typeToken.definition.left.get, typeToken.verifications)
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
