package definiti.parser

import definiti._
import definiti.api.{Core, TypeReference}

object ASTValidation {
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
