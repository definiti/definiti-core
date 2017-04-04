package definiti

import definiti.utils.NumberUtils

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SyntaxEnhancer {
  type Syntax = Seq[SyntaxToken]
  
  def enhanceSyntax(source: Syntax): Syntax = {
    val withEnclosing = buildEnclosing(source)
    val withEOLIgnored = ignoreEOLInEncloser(withEnclosing)
    val withFirstClassCitizen = buildFirstClassCitizen(withEOLIgnored)
    val withFunctions = buildFunctions(withFirstClassCitizen)
    val withConditions = buildConditions(withFunctions)
    val withFirstClassCitizenCompleted = completeFirstClassCitizenStructure(withConditions)
    val withExpressions = buildExpressions(withFirstClassCitizenCompleted)
    withExpressions
  }

  private[definiti] def buildEnclosing(source: Syntax): Syntax = {
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

  private[definiti] def removeEOLFromParenthesis(source: Syntax): Syntax = {
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

  private[definiti] def trimEOLFromBrace(source: Syntax): Syntax = {
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

  private[definiti] def ignoreEOLInEncloser(source: Syntax): Syntax = {
    trimEOLFromBrace(removeEOLFromParenthesis(trimEOL(source)))
  }

  private[definiti] def trimEOL(source: Syntax): Syntax = {
    source.dropWhile(_ == EndOfLine).reverse.dropWhile(_ == EndOfLine).reverse
  }

  private[definiti] def buildFirstClassCitizen(source: Syntax): Syntax = {
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

  private[definiti] def buildFunctions(source: Syntax): Syntax = {
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

  private[definiti] def extractParameterDefinition(source: ParenthesisExpressionToken): Seq[FunctionParameter] = {
    @tailrec
    def process(acc: Seq[FunctionParameter], remainingSyntax: Syntax): Seq[FunctionParameter] = remainingSyntax match {
      case Nil => acc
      case Word(name) :: Colon :: Word(typeReference) :: tail => process(acc :+ FunctionParameter(name, typeReference), tail)
      case Comma :: tail => process(acc, tail)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }
    process(Nil, source.children.filter(_ != EndOfLine))
  }

  private[definiti] def buildConditions(source: Syntax): Syntax = {
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

  private[definiti] def completeFirstClassCitizenStructure(syntax: Syntax): Syntax = {
    syntax.map {
      case token: VerificationToken => completeVerificationToken(token)
      case token: TypeToken => completeTypeToken(token)
      case token => token
    }
  }

  private[definiti] def completeVerificationToken(verificationToken: VerificationToken): StructuredVerificationToken = {
    verificationToken.body.children match {
      case QuotedString(message) :: EndOfLine :: (function: FunctionToken) :: Nil =>
        StructuredVerificationToken(verificationToken.name, message, function)
      case token => throw new RuntimeException("Unexpected token: " + token)
    }
  }

  private[definiti] def completeTypeToken(typeToken: TypeToken): StructuredTypeToken = {
    typeToken.definition match {
      case Left(_) => completeAliasTypeToken(typeToken)
      case Right(_) => completeDefinedTypeToken(typeToken)
    }
  }

  private[definiti] def completeDefinedTypeToken(typeToken: TypeToken): StructuredDefinedTypeToken = {
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

  private[definiti] def completeAliasTypeToken(typeToken: TypeToken): StructuredAliasTypeToken = {
    StructuredAliasTypeToken(typeToken.name, typeToken.definition.left.get, typeToken.verifications)
  }

  private[definiti] def buildExpressions(syntax: Syntax): Syntax = {
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

    def splitBraceByEOL(brace: BraceExpressionToken): Seq[Syntax] = {
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

      process(Nil, Nil, trimEOL(removeComments(brace.children)))
    }

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
          case Word(_) :: Dot :: _ =>
            chainedCalls(syntax)
          case (condition: ConditionToken) :: Nil =>
            ConditionExpressionToken(
              condition = buildExpression(condition.condition.children),
              onTrue = CombinedExpressionToken(splitBraceByEOL(condition.onTrue).map(buildExpression)).simplify(),
              onFalse = condition.onFalse.map(body => CombinedExpressionToken(splitBraceByEOL(body).map(buildExpression)).simplify())
            )
          case token => throw new RuntimeException("Unexpected token: " + token)
        }
      }
      def process(syntaxTree: SyntaxTree): ExpressionToken = syntaxTree match {
        case SyntaxTreeBranch(symbol, left, right) =>
          symbol.toExpressionToken(process(left), process(right))
        case SyntaxTreeLeaf(innerSyntax) => buildFinalExpression(innerSyntax)
      }
      process(createSyntaxTreeWithSymbols(syntax))
    }

    def process(acc: Syntax, remaining: Syntax): Syntax = remaining match {
      case Nil =>
        acc
      case (function: FunctionToken) :: tail =>
        val newToken = FunctionExpressionToken(
          parameters = function.parameters,
          body = CombinedExpressionToken(splitBraceByEOL(function.body).map(buildExpression)).simplify()
        )
        process(acc :+ newToken, tail)
      case (structuredVerificationToken: StructuredVerificationToken) :: tail =>
        val newToken = VerificationExpressionToken(
          structuredVerificationToken.name,
          structuredVerificationToken.message,
          FunctionExpressionToken(
            parameters = structuredVerificationToken.function.parameters,
            body = CombinedExpressionToken(splitBraceByEOL(structuredVerificationToken.function.body).map(buildExpression)).simplify()
          )
        )
        process(acc :+ newToken, tail)
      case (structuredDefinedTypeToken: StructuredDefinedTypeToken) :: tail =>
        val newToken = DefinedTypeExpressionToken(
          structuredDefinedTypeToken.name,
          structuredDefinedTypeToken.fields,
          structuredDefinedTypeToken.definedVerifications.map { definedVerification =>
            TypeVerificationExpressionToken(
              definedVerification.message,
              FunctionExpressionToken(
                parameters = definedVerification.function.parameters,
                body = CombinedExpressionToken(splitBraceByEOL(definedVerification.function.body).map(buildExpression)).simplify()
              )
            )
          },
          structuredDefinedTypeToken.verifications
        )
        process(acc :+ newToken, tail)
      case (containerToken: ContainerToken[_]) :: tail =>
        process(acc :+ containerToken.mapOnContainers(buildExpressions), tail)
      case token :: tail =>
        process(acc :+ token, tail)
    }

    process(Nil, syntax)
  }
}
