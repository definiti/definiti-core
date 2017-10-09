package definiti.core

import definiti.core.ast._
import definiti.core.ast.pure._

private[core] case class ClassReference(classDefinition: PureClassDefinition, genericTypes: Seq[ClassReference])

// TODO: This system need a refactoring, lot of code is duplicated.
private[core] sealed trait Context {
  def isTypeAvailable(typeName: String): Boolean

  def findType(typeName: String): Option[PureClassDefinition]

  def isVerificationAvailable(verificationName: String): Boolean

  def isVerificationAvailable(verificationReference: VerificationReference): Boolean = {
    isVerificationAvailable(verificationReference.verificationName)
  }

  def findVerification(verificationName: String): Option[PureVerification]

  def findVerification(verificationReference: VerificationReference): Option[PureVerification] = {
    findVerification(verificationReference.verificationName)
  }

  def isFunctionAvailable(functionName: String): Boolean

  def findFunction(functionName: String): Option[PureNamedFunction]

  def findTypeReference(name: String): Option[AbstractTypeReference]
}

private[core] case class ReferenceContext(
  classes: Seq[PureClassDefinition],
  verifications: Seq[PureVerification],
  namedFunctions: Seq[PureNamedFunction]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    classes.exists(_.canonicalName == typeName)
  }

  override def findType(typeName: String): Option[PureClassDefinition] = {
    classes.find(_.canonicalName == typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    verifications.exists(_.canonicalName == verificationName)
  }

  override def findVerification(verificationName: String): Option[PureVerification] = {
    verifications.find(_.canonicalName == verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    namedFunctions.exists(_.canonicalName == functionName)
  }

  override def findFunction(functionName: String): Option[PureNamedFunction] = {
    namedFunctions.find(_.canonicalName == functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    namedFunctions
      .find(_.canonicalName == name)
      .map(_ => NamedFunctionReference(name))
  }
}

private[core] case class ClassContext(
  outerContext: Context,
  currentType: PureClassDefinition,
  genericTypes: Seq[ClassReference]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentType.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[PureClassDefinition] = {
    if (currentType.genericTypes.contains(typeName)) {
      val indexOfGeneric = currentType.genericTypes.indexOf(typeName)
      if (indexOfGeneric < genericTypes.size) {
        outerContext.findType(genericTypes(indexOfGeneric).classDefinition.name)
      } else {
        None
      }
    } else {
      outerContext.findType(typeName)
    }
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[PureVerification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[PureNamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    outerContext.findTypeReference(name)
  }
}

private[core] case class MethodContext(
  outerContext: Context,
  currentMethod: MethodDefinition,
  genericTypes: Seq[ClassReference]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentMethod.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[PureClassDefinition] = {
    if (currentMethod.genericTypes.contains(typeName)) {
      val indexOfGeneric = currentMethod.genericTypes.indexOf(typeName)
      if (indexOfGeneric < genericTypes.size) {
        outerContext.findType(genericTypes(indexOfGeneric).classDefinition.name)
      } else {
        None
      }
    } else {
      outerContext.findType(typeName)
    }
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[PureVerification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[PureNamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    currentMethod.parameters
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

private[core] case class DefinedFunctionContext(
  outerContext: Context,
  currentFunction: PureDefinedFunction
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentFunction.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[PureClassDefinition] = {
    outerContext.findType(typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[PureVerification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[PureNamedFunction] = {
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
  currentFunction: PureNamedFunction
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentFunction.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[PureClassDefinition] = {
    if (currentFunction.genericTypes.contains(typeName)) {
      // There is neither covariance nor contravariance in the language, so the generic type is any type.
      Some(PureNativeClassDefinition(
        name = "A",
        genericTypes = Seq.empty,
        attributes = Seq.empty,
        methods = Seq.empty,
        comment = None
      ))
    } else {
      outerContext.findType(typeName)
    }
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[PureVerification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[PureNamedFunction] = {
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
  lambda: PureLambdaExpression
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[PureClassDefinition] = {
    outerContext.findType(typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[PureVerification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[PureNamedFunction] = {
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