package definiti.core

import definiti.common.ast._

private[core] case class ClassReference(classDefinition: ClassDefinition, genericTypes: Seq[ClassReference])

// TODO: This system need a refactoring, lot of code is duplicated.
private[core] sealed trait Context {
  def findType(typeName: String): Option[ClassDefinition]

  def findFunction(functionName: String): Option[NamedFunction]

  def findTypeReference(name: String): Option[AbstractTypeReference]
}

private[core] case class ReferenceContext(
  classes: Seq[ClassDefinition],
  namedFunctions: Seq[NamedFunction]
) extends Context {
  override def findType(typeName: String): Option[ClassDefinition] = {
    classes.find(_.fullName == typeName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    namedFunctions.find(_.fullName == functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    findClassReference(name).orElse(findNamedFunctionReference(name))
  }

  private def findClassReference(name: String): Option[AbstractTypeReference] = {
    classes
      .find(_.fullName == name)
      .map(_ => TypeReference(name))
  }

  private def findNamedFunctionReference(name: String): Option[AbstractTypeReference] = {
    namedFunctions
      .find(_.fullName == name)
      .map(_ => NamedFunctionReference(name))
  }
}

private[core] case class ClassContext(
  outerContext: Context,
  currentType: ClassDefinition
) extends Context {
  override def findType(typeName: String): Option[ClassDefinition] = {
    if (currentType.genericTypes.contains(typeName)) {
      // There is neither covariance nor contravariance in the language, so the generic type is any type.
      Some(NativeClassDefinition(
        name = "A",
        fullName = "A",
        genericTypes = Seq.empty,
        attributes = Seq.empty,
        methods = Seq.empty,
        comment = None
      ))
    } else {
      outerContext.findType(typeName)
    }
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    val classParameters = currentType match {
      case aliasType: AliasType => aliasType.parameters
      case definedType: DefinedType => definedType.parameters
      case _ => Seq.empty
    }
    classParameters
      .find(_.name == name)
      .map(_.typeReference)
      .orElse(outerContext.findTypeReference(name))
  }
}

private[core] case class VerificationContext(
  outerContext: Context,
  currentVerification: Verification
) extends Context {
  override def findType(typeName: String): Option[ClassDefinition] = outerContext.findType(typeName)

  override def findFunction(functionName: String): Option[NamedFunction] = outerContext.findFunction(functionName)

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    currentVerification.parameters
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => Some(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti verifications
        }
      }
      .orElse(outerContext.findTypeReference(name))
  }
}

private[core] case class DefinedFunctionContext(
  outerContext: Context,
  currentFunction: DefinedFunction
) extends Context {
  override def findType(typeName: String): Option[ClassDefinition] = {
    if (currentFunction.genericTypes.contains(typeName)) {
      // There is neither covariance nor contravariance in the language, so the generic type is any type.
      Some(NativeClassDefinition(
        name = "A",
        fullName = "A",
        genericTypes = Seq.empty,
        attributes = Seq.empty,
        methods = Seq.empty,
        comment = None
      ))
    } else {
      outerContext.findType(typeName)
    }
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    currentFunction.parameters
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => Some(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti functions
        }
      }
      .orElse(outerContext.findTypeReference(name))
  }
}

private[core] case class NamedFunctionReferenceContext(
  outerContext: Context,
  currentFunction: NamedFunction
) extends Context {
  override def findType(typeName: String): Option[ClassDefinition] = {
    if (currentFunction.genericTypes.contains(typeName)) {
      // There is neither covariance nor contravariance in the language, so the generic type is any type.
      Some(NativeClassDefinition(
        name = "A",
        fullName = "A",
        genericTypes = Seq.empty,
        attributes = Seq.empty,
        methods = Seq.empty,
        comment = None
      ))
    } else {
      outerContext.findType(typeName)
    }
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    currentFunction.parameters
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => Some(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti functions
        }
      }
      .orElse(outerContext.findTypeReference(name))
  }
}

private[core] case class LambdaContext(
  outerContext: Context,
  lambda: LambdaExpression
) extends Context {
  override def findType(typeName: String): Option[ClassDefinition] = {
    outerContext.findType(typeName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    lambda.parameterList
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => Some(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti functions
        }
      }
      .orElse(outerContext.findTypeReference(name))
  }
}