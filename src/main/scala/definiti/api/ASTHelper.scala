package definiti.api

import definiti._

object ASTHelper {
  def getReturnTypeOptOfMethod(methodDefinition: MethodDefinition)(implicit context: Context): Option[ClassDefinition] = {
    methodDefinition match {
      case nativeMethodDefinition: NativeMethodDefinition =>
        context.findType(nativeMethodDefinition.returnTypeReference)
      case definedMethodDefinition: DefinedMethodDefinition =>
        getReturnTypeOptOfExpression(definedMethodDefinition.function.body)
    }
  }

  def getReturnTypeOfMethod(methodDefinition: MethodDefinition)(implicit context: Context): ClassDefinition = {
    getReturnTypeOptOfMethod(methodDefinition).get
  }

  def getReturnTypeOptOfExpression(expression: Expression)(implicit context: Context): Option[ClassDefinition] = {
    expression match {
      case _: LogicalExpression => context.findType("Boolean")
      case _: CalculatorExpression => context.findType("Number")
      case NumberValue(_, _) => context.findType("Number")
      case QuotedStringValue(_, _) => context.findType("String")
      case Variable(_, typeReference, _) => context.findType(typeReference)
      case MethodCall(innerExpression, method, _, _) =>
        getReturnTypeOptOfExpression(innerExpression).flatMap { expressionType =>
          getMethodOpt(expressionType, method).flatMap { method =>
            getReturnTypeOptOfMethod(method)
          }
        }
      case AttributeCall(innerExpression, attribute, _) =>
        getReturnTypeOptOfExpression(innerExpression).flatMap { expressionType =>
          getAttributeOpt(expressionType, attribute)
            .map(_.typeReference)
            .flatMap(context.findType)
        }
      case CombinedExpression(parts, _) =>
        parts.lastOption.flatMap(getReturnTypeOptOfExpression)
      case Condition(_, onTrue, onFalseOpt, _) =>
        onFalseOpt match {
          case Some(onFalse) =>
            (
              getReturnTypeOptOfExpression(onTrue),
              getReturnTypeOptOfExpression(onFalse)
            ) match {
              case (Some(onTrueType), Some(onFalseType)) =>
                if (onTrueType.name == onFalseType.name) {
                  Some(onTrueType)
                } else {
                  Some(Core.any)
                }
              case _ => None
            }
          case _ => Some(Core.unit)
        }
    }
  }

  def getReturnTypeOfExpression(expression: Expression)(implicit context: Context): ClassDefinition = {
    getReturnTypeOptOfExpression(expression).get
  }

  def getMethodOpt(classDefinition: ClassDefinition, method: String)(implicit context: Context): Option[MethodDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.methods.find(_.name == method)
      case aliasType: AliasType =>
        context.findType(aliasType.alias).flatMap(getMethodOpt(_, method))
      case definedType: DefinedType =>
        definedType.methods.find(_.name == method)
    }
  }

  def getAttributeOpt(classDefinition: ClassDefinition, attribute: String)(implicit context: Context): Option[AttributeDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.attributes.find(_.name == attribute)
      case aliasType: AliasType =>
        context.findType(aliasType.alias).flatMap(getAttributeOpt(_, attribute))
      case definedType: DefinedType =>
        definedType.attributes.find(_.name == attribute)
    }
  }
}
