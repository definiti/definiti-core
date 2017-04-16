package definiti.parser

import definiti._
import definiti.api.{Core, TypeReference}

sealed trait Validation {
  def join(other: Validation): Validation

  def verifyingAlso(nextValidation: => Validation): Validation
}

object Validation {
  def join(validations: Seq[Validation]): Validation = {
    validations.foldLeft(Valid.asInstanceOf[Validation]) { (acc, validation) => acc.join(validation) }
  }

  def join(validations: Validation*)(implicit dummyImplicit: DummyImplicit): Validation = {
    join(validations)
  }
}

case object Valid extends Validation {
  override def join(other: Validation): Validation = other

  override def verifyingAlso(nextValidation: => Validation): Validation = nextValidation
}

case class Invalid(errors: Seq[Error]) extends Validation {
  def join(other: Validation): Validation = other match {
    case Valid => this
    case Invalid(otherErrors) => Invalid(errors ++ otherErrors)
  }

  override def verifyingAlso(nextValidation: => Validation): Validation = this
}

object Invalid {
  def apply(message: String, range: Range): Invalid = new Invalid(Seq(Error(message, range)))
}

case class Error(message: String, range: Range)

object ASTValidation {
  def validate(root: Root): Validation = {
    val verificationValidations = root.verifications.map { verification =>
      validateExpression(verification.function.body).verifyingAlso {
        if (verification.function.returnType == Core.boolean) {
          Valid
        } else {
          Invalid("The function in verification must be a boolean expression, got: " + verification.function.returnType.name, verification.function.body.range)
        }
      }
    }

    val classDefinitionValidations = root.classDefinitions.map {
      case aliasType: AliasType =>
        verifyTypeReference(aliasType).verifyingAlso {
          Validation.join(aliasType.inherited.map { verification =>
            TypeReference.findVerification(verification) match {
              case Some(_) => Valid
              case None => Invalid("Undefined verification: " + verification, aliasType.range)
            }
          })
        }
      case definedType: DefinedType =>
        val inheritedValidations = definedType.inherited.map { verification =>
          TypeReference.findVerification(verification) match {
            case Some(_) => Valid
            case None => Invalid("Undefined verification: " + verification, definedType.range)
          }
        }
        val attributeValidations = definedType.attributes.map { attribute =>
          TypeReference.findType(attribute.typeReference) match {
            case Some(_) => Valid
            case None => Invalid("Undefined type: " + attribute.typeReference, attribute.range)
          }
        }
        val verificationValidations = definedType.verifications.map { verification =>
          validateExpression(verification.function.body).verifyingAlso {
            if (verification.function.returnType == Core.boolean) {
              Valid
            } else {
              Invalid("The function in verification must be a Boolean, got: " + verification.function.returnType.name, verification.function.range)
            }
          }
        }
        Validation.join(inheritedValidations ++ attributeValidations ++ verificationValidations)
      case _ => Valid
    }

    Validation.join(verificationValidations ++ classDefinitionValidations)
  }

  private[definiti] def validateExpression(expression: Expression): Validation = expression match {
    case BooleanValue(_, _) => Valid
    case NumberValue(_, _) => Valid
    case QuotedStringValue(_, _) => Valid
    case Variable(_, typeReference, range) =>
      TypeReference.findType(typeReference) match {
        case Some(_) => Valid
        case None => Invalid("Unknown type: " + typeReference, range)
      }
    case MethodCall(inner, method, parameters, range) =>
      validateExpression(inner).verifyingAlso {
        val methodNameValidation = inner.returnType.methods.find(_.name == method) match {
          case Some(_) => Valid
          case None => Invalid(s"Unknown method ${inner.returnType.name}.$method", range)
        }
        val parameterValidations = parameters.map(validateExpression)
        Validation.join(methodNameValidation +: parameterValidations)
      }
    case AttributeCall(inner, attribute, range) =>
      validateExpression(inner).verifyingAlso {
        inner.returnType.attributes.find(_.name == attribute) match {
          case Some(_) => Valid
          case None => Invalid(s"Unknown attribute ${inner.returnType.name}.$attribute", range)
        }
      }
    case CombinedExpression(parts, _) =>
      Validation.join(parts.map(validateExpression))
    case Condition(condition, onTrue, onFalse, _) =>
      validateExpression(condition)
      validateExpression(onTrue)
      onFalse.foreach(validateExpression)
      if (condition.returnType == Core.boolean) {
        Valid
      } else {
        Invalid("The condition must be a boolean expression, got: " + condition.returnType.name, condition.range)
      }
    case Or(left, right, _) =>
      validateBooleanExpression(left, right)
    case And(left, right, _) =>
      validateBooleanExpression(left, right)
    case Equal(left, right, _) =>
      validateExpressions(left, right)
    case NotEqual(left, right, _) =>
      validateExpressions(left, right)
    case Lower(left, right, _) =>
      validateExpressions(left, right)
    case Upper(left, right, _) =>
      validateExpressions(left, right)
    case LowerOrEqual(left, right, _) =>
      validateExpressions(left, right)
    case UpperOrEqual(left, right, _) =>
      validateExpressions(left, right)
    case Plus(left, right, _) =>
      validateExpressions(left, right)
    case Minus(left, right, _) =>
      validateExpressions(left, right)
    case Modulo(left, right, _) =>
      validateExpressions(left, right)
    case Time(left, right, _) =>
      validateExpressions(left, right)
    case Divide(left, right, _) =>
      validateExpressions(left, right)
    case Not(inner, _) =>
      validateExpression(inner).verifyingAlso {
        if (inner.returnType == Core.boolean) {
          Valid
        } else {
          Invalid("The expression must be a boolean expression, got: " + inner.returnType.name, inner.range)
        }
      }
  }

  private def validateExpressions(expressions: Expression*): Validation = {
    Validation.join(expressions.map(validateExpression))
  }

  private def validateBooleanExpression(left: Expression, right: Expression): Validation = {
    validateExpressions(left, right).verifyingAlso {
      if (left.returnType != Core.boolean) {
        Invalid("The left part of logical expression must be a boolean expression, got: " + left.returnType.name, left.range)
      } else if (right.returnType != Core.boolean) {
        Invalid("The right part of logical expression must be a boolean expression, got: " + right.returnType.name, right.range)
      } else {
        Valid
      }
    }
  }

  private def verifyTypeReference(aliasType: AliasType) = {
    TypeReference.findType(aliasType.alias) match {
      case Some(_) => Valid
      case None => Invalid("Undefined type: " + aliasType.alias, aliasType.range)
    }
  }
}
