package definiti.api

import definiti._

case class ClassReference(classDefinition: ClassDefinition, genericTypes: Seq[ClassReference])

object ASTHelper {
  def getReturnTypeOptOfMethod(methodDefinition: MethodDefinition)(implicit context: Context): Option[ClassReference] = {
    methodDefinition match {
      case nativeMethodDefinition: NativeMethodDefinition =>
        getClassReference(nativeMethodDefinition.returnTypeReference)
      case definedMethodDefinition: DefinedMethodDefinition =>
        getReturnTypeOptOfExpression(definedMethodDefinition.function.body)
    }
  }

  def getReturnTypeOfMethod(methodDefinition: MethodDefinition)(implicit context: Context): ClassReference = {
    getReturnTypeOptOfMethod(methodDefinition).get
  }

  def getReturnTypeOptOfExpression(expression: Expression)(implicit context: Context): Option[ClassReference] = {
    expression match {
      case _: LogicalExpression => context.findType("Boolean").map(ClassReference(_, Seq()))
      case _: CalculatorExpression => context.findType("Number").map(ClassReference(_, Seq()))
      case NumberValue(_, _) => context.findType("Number").map(ClassReference(_, Seq()))
      case QuotedStringValue(_, _) => context.findType("String").map(ClassReference(_, Seq()))
      case Variable(_, typeReference, _) =>
        getClassReference(typeReference)
      case methodCall: MethodCall =>
        getReturnTypeOfMethodCall(methodCall)
      case attributeCall: AttributeCall =>
        getReturnTypeOfAttributeCall(attributeCall)
      case CombinedExpression(parts, _) =>
        parts.lastOption.flatMap(getReturnTypeOptOfExpression)
      case Condition(_, onTrue, onFalseOpt, _) =>
        getReturnTypeOptOfCondition(onTrue, onFalseOpt)
    }
  }

  private def getReturnTypeOfMethodCall(methodCall: MethodCall)(implicit context: Context) = {
    getReturnTypeOptOfExpression(methodCall.expression).flatMap { expressionType =>
      getMethodOpt(expressionType.classDefinition, methodCall.method).flatMap { method =>
        val classContext = ClassContext(
          context,
          currentType = expressionType.classDefinition,
          expressionType.genericTypes
        )
        getReturnTypeOptOfMethod(method)(classContext)
      }
    }
  }

  private def getReturnTypeOfAttributeCall(attributeCall: AttributeCall)(implicit context: Context): Option[ClassReference] = {
    getReturnTypeOptOfExpression(attributeCall.expression).flatMap { expressionType =>
      getAttributeOpt(expressionType.classDefinition, attributeCall.attribute).flatMap { attributeDefinition =>
        val classContext = ClassContext(
          context,
          currentType = expressionType.classDefinition,
          expressionType.genericTypes
        )
        getClassReference(attributeDefinition.typeReference)(classContext)
      }
    }
  }

  private def getReturnTypeOptOfCondition(onTrue: Expression, onFalseOpt: Option[Expression])(implicit context: Context) = {
    onFalseOpt match {
      case Some(onFalse) =>
        (
          getReturnTypeOptOfExpression(onTrue),
          getReturnTypeOptOfExpression(onFalse)
        ) match {
          case (Some(onTrueType), Some(onFalseType)) =>
            if (onTrueType.classDefinition.name == onFalseType.classDefinition.name) {
              Some(onTrueType)
            } else {
              Some(ClassReference(Core.any, Seq()))
            }
          case _ => None
        }
      case _ => Some(ClassReference(Core.unit, Seq()))
    }
  }

  private def getClassReference(typeReference: TypeReference)(implicit context: Context): Option[ClassReference] = {
    val classReferenceOpt = context.findType(typeReference.typeName)
    val genericClassReferenceOpts = typeReference.genericTypes.map(getClassReference(_).getOrElse(ClassReference(Core.any, Seq())))
    if (classReferenceOpt.nonEmpty) {
      Some(ClassReference(
        classDefinition = classReferenceOpt.get,
        genericTypes = genericClassReferenceOpts
      ))
    } else {
      None
    }
  }

  def getReturnTypeOfExpression(expression: Expression)(implicit context: Context): ClassReference = {
    getReturnTypeOptOfExpression(expression).get
  }

  def getMethodOpt(classDefinition: ClassDefinition, method: String)(implicit context: Context): Option[MethodDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.methods.find(_.name == method)
      case aliasType: AliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getMethodOpt(_, method))
      case definedType: DefinedType =>
        definedType.methods.find(_.name == method)
    }
  }

  def getAttributeOpt(classDefinition: ClassDefinition, attribute: String)(implicit context: Context): Option[AttributeDefinition] = {
    classDefinition match {
      case nativeClassDefinition: NativeClassDefinition =>
        nativeClassDefinition.attributes.find(_.name == attribute)
      case aliasType: AliasType =>
        context.findType(aliasType.alias.typeName).flatMap(getAttributeOpt(_, attribute))
      case definedType: DefinedType =>
        definedType.attributes.find(_.name == attribute)
    }
  }
}
