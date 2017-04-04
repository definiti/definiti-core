package definiti

import definiti.api.{Core, TypeReference}

import scala.collection.mutable.ListBuffer

case class Scope(variables: Seq[Variable])

object Scope {
  val empty = Scope(Seq())
}

object SyntaxToAstProcessor {
  type Syntax = Seq[SyntaxToken]

  def transform(syntax: Syntax): Root = {
    val verificationsBuffer = ListBuffer[Verification]()
    val typesBuffer = ListBuffer[Type]()

    syntax.foldLeft(Option[String](null)) { (previousComment, token) =>
      token match {
        case LineComment(comment) =>
          Some(comment)
        case BlockComment(comment) =>
          Some(comment)
        case verification: VerificationExpressionToken =>
          verificationsBuffer.append(Verification(
            verification.name,
            verification.message,
            function = processFunction(verification.function, Scope.empty),
            previousComment
          ))
          None
        case definedType: DefinedTypeExpressionToken =>
          typesBuffer.append(DefinedType(
            definedType.name,
            definedType.fields.map(f => AttributeDefinition(f.name, f.typeReference, None)),
            definedType.definedVerifications.map { definedVerification =>
              TypeVerification(
                definedVerification.message,
                processFunction(definedVerification.function, Scope.empty)
              )
            },
            definedType.verifications,
            previousComment
          ))
          None
        case aliasType: StructuredAliasTypeToken =>
          typesBuffer.append(AliasType(
            aliasType.name,
            aliasType.alias,
            aliasType.verifications,
            previousComment
          ))
          None
        case _ => throw new RuntimeException("Unexpected token: " + token)
      }
    }

    Root(verificationsBuffer, typesBuffer)
  }

  def injectIntoReference(root: Root): Unit = {
    root.verifications.foreach(TypeReference.referenceVerification)
    root.classDefinitions.foreach(TypeReference.referenceType)
  }

  def validate(root: Root): Unit = {
    // Will throw an exception on error.
    root.verifications.foreach { verification =>
      validateExpression(verification.function.body)
      if (verification.function.returnType != Core.boolean) {
        throw new RuntimeException("The function in verification must be a Boolean, got: " + verification.function.returnType.name)
      }
    }

    root.classDefinitions.foreach {
      case aliasType: AliasType =>
        TypeReference.findType(aliasType.alias) match {
          case Some(_) => // valid
          case None => throw new RuntimeException("Undefined type: " + aliasType.alias)
        }
        aliasType.inherited.foreach { verification =>
          TypeReference.findVerification(verification) match {
            case Some(_) => //valid
            case None => throw new RuntimeException("Undefined verification: " + verification)
          }
        }
      case definedType: DefinedType =>
        definedType.inherited.foreach { verification =>
          TypeReference.findVerification(verification) match {
            case Some(_) => //valid
            case None => throw new RuntimeException("Undefined verification: " + verification)
          }
        }
        definedType.attributes.foreach { attribute =>
          TypeReference.findType(attribute.typeReference) match {
            case Some(_) => //valid
            case None => throw new RuntimeException("Undefined type: " + attribute.typeReference)
          }
        }
        definedType.verifications.foreach { verification =>
          validateExpression(verification.function.body)
          if (verification.function.returnType != Core.boolean) {
            throw new RuntimeException("The function in verification must be a Boolean, got: " + verification.function.returnType.name)
          }
        }
      case _ => // valid
    }
  }

  private[definiti] def processFunction(functionToken: FunctionExpressionToken, scope: Scope): DefinedFunction = {
    val innerVariables = ListBuffer[Variable]()
    functionToken.parameters.foreach(p => innerVariables.append(Variable(p.name, p.typeReference)))
    scope.variables.foreach { v =>
      if (!innerVariables.exists(_.name == v.name)) {
        innerVariables.append(v)
      }
    }
    DefinedFunction(
      functionToken.parameters.map(p => ParameterDefinition(p.name, p.typeReference)),
      body = processExpressionToken(functionToken.body, Scope(innerVariables))
    )
  }

  private[definiti] def processExpressionToken(expressionToken: ExpressionToken, scope: Scope): Expression = expressionToken match {
    case BooleanExpressionToken(value) =>
      BooleanValue(value)
    case NumberExpressionToken(value) =>
      NumberValue(value)
    case QuotedStringExpressionToken(value) =>
      QuotedStringValue(value)
    case VariableExpressionToken(variableName) =>
      scope.variables.find(_.name == variableName) match {
        case Some(variable) => Variable(variableName, variable.typeReference)
        case None => throw new RuntimeException("Undefined variable: " + variableName)
      }
    case MethodCallToken(expression, method, parameters) =>
      MethodCall(processExpressionToken(expression, scope), method, parameters.parts.map(part => processExpressionToken(part, scope)))
    case AttributeCallToken(expression, attribute) =>
      AttributeCall(processExpressionToken(expression, scope), attribute)
    case CombinedExpressionToken(parts) =>
      CombinedExpression(parts.map(part => processExpressionToken(part, scope)))
    case ConditionExpressionToken(condition, onTrue, onFalse) =>
      Condition(
        processExpressionToken(condition, scope),
        processExpressionToken(onTrue, scope),
        onFalse.map(body => processExpressionToken(body, scope))
      )
    case OrExpression(left, right) =>
      Or(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case AndExpression(left, right) =>
      And(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case EqualExpression(left, right) =>
      Equal(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case NotEqualExpression(left, right) =>
      NotEqual(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case LowerExpression(left, right) =>
      Lower(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case UpperExpression(left, right) =>
      Upper(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case LowerOrEqualExpression(left, right) =>
      LowerOrEqual(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case UpperOrEqualExpression(left, right) =>
      UpperOrEqual(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case PlusExpression(left, right) =>
      Plus(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case MinusExpression(left, right) =>
      Minus(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case ModuloExpression(left, right) =>
      Modulo(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case TimeExpression(left, right) =>
      Time(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case DivideExpression(left, right) =>
      Divide(processExpressionToken(left, scope), processExpressionToken(right, scope))
    case NotExpression(inner) =>
      Not(processExpressionToken(inner, scope))
  }

  private[definiti] def validateExpression(expression: Expression): Unit = expression match {
    case BooleanValue(_) => // valid
    case NumberValue(_) => // valid
    case QuotedStringValue(_) => // valid
    case Variable(_, typeReference) =>
      TypeReference.findType(typeReference) match {
        case Some(_) => // valid
        case None => throw new RuntimeException("Unknown type: " + typeReference)
      }
    case MethodCall(inner, method, parameters) =>
      validateExpression(inner)
      inner.returnType.methods.find(_.name == method) match {
        case Some(_) => // valid
        case None => throw new RuntimeException(s"Unknown method ${inner.returnType.name}.$method")
      }
      parameters.foreach(validateExpression)
    case AttributeCall(inner, attribute) =>
      validateExpression(inner)
      inner.returnType.attributes.find(_.name == attribute) match {
        case Some(_) => // valid
        case None => throw new RuntimeException(s"Unknown attribute ${inner.returnType.name}.$attribute")
      }
    case CombinedExpression(parts) =>
      parts.foreach(validateExpression)
    case Condition(condition, onTrue, onFalse) =>
      validateExpression(condition)
      validateExpression(onTrue)
      onFalse.foreach(validateExpression)
      if (condition.returnType != Core.boolean) {
        throw new RuntimeException("The condition must be a boolean expression, got: " + condition.returnType.name)
      }
    case Or(left, right) =>
      validateBooleanExpression(left, right)
    case And(left, right) =>
      validateBooleanExpression(left, right)
    case Equal(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case NotEqual(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Lower(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Upper(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case LowerOrEqual(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case UpperOrEqual(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Plus(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Minus(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Modulo(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Time(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Divide(left, right) =>
      validateExpression(left)
      validateExpression(right)
    case Not(inner) =>
      validateExpression(inner)
      if (inner.returnType != Core.boolean) {
        throw new RuntimeException("The left part of logical expression must be a boolean expression, got: " + inner.returnType.name)
      }
  }

  private[definiti] def validateBooleanExpression(left: Expression, right: Expression) = {
    validateExpression(left)
    validateExpression(right)
    if (left.returnType != Core.boolean) {
      throw new RuntimeException("The left part of logical expression must be a boolean expression, got: " + left.returnType.name)
    }
    if (right.returnType != Core.boolean) {
      throw new RuntimeException("The left part of logical expression must be a boolean expression, got: " + left.returnType.name)
    }
  }
}
