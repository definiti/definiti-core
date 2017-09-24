package definiti.core

import definiti.core.ast.pure._
import definiti.core.utils.Core

// TODO: This system need a refactoring, lot of code is duplicated.
sealed trait Context {
  def isTypeAvailable(typeName: String): Boolean

  def findType(typeName: String): Option[ClassDefinition]

  def isVerificationAvailable(verificationName: String): Boolean

  def isVerificationAvailable(verificationReference: VerificationReference): Boolean = {
    isVerificationAvailable(verificationReference.verificationName)
  }

  def findVerification(verificationName: String): Option[Verification]

  def findVerification(verificationReference: VerificationReference): Option[Verification] = {
    findVerification(verificationReference.verificationName)
  }

  def isFunctionAvailable(functionName: String): Boolean

  def findFunction(functionName: String): Option[NamedFunction]

  def isReferencesAvailable(name: String): Boolean = findReference(name).nonEmpty

  def findReference(name: String): Option[ElementReference]

  def findTypeReference(name: String): Option[AbstractTypeReference]

  def isRequirementAvailable(name: String): Boolean = {
    findReference(name).isDefined
  }
}

case class ReferenceContext(
  classes: Seq[ClassDefinition],
  verifications: Seq[Verification],
  namedFunctions: Seq[NamedFunction]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    classes.exists(_.canonicalName == typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
    classes.find(_.canonicalName == typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    verifications.exists(_.canonicalName == verificationName)
  }

  override def findVerification(verificationName: String): Option[Verification] = {
    verifications.find(_.canonicalName == verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    namedFunctions.exists(_.canonicalName == functionName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    namedFunctions.find(_.canonicalName == functionName)
  }

  override def findReference(name: String): Option[ElementReference] = {
    namedFunctions
      .find(_.canonicalName == name)
      .map(NamedFunctionReference)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    namedFunctions
      .find(_.canonicalName == name)
      .map(_.returnType)
  }
}

case class ClassContext(
  outerContext: Context,
  currentType: ClassDefinition,
  genericTypes: Seq[ClassReference]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentType.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
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

  override def findVerification(verificationName: String): Option[Verification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findReference(name: String): Option[ElementReference] = {
    outerContext.findReference(name)
  }

  override def findTypeReference(name: String): Option[AbstractTypeReference] = {
    outerContext.findTypeReference(name)
  }
}

case class MethodContext(
  outerContext: Context,
  currentMethod: MethodDefinition,
  genericTypes: Seq[ClassReference]
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentMethod.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
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

  override def findVerification(verificationName: String): Option[Verification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findReference(name: String): Option[ElementReference] = {
    currentMethod.parameters
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => getClassReference(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti functions
        }
      }
      .orElse(outerContext.findReference(name))
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

  private def getClassReference(typeReference: TypeReference): Option[ClassReference] = {
    val classReferenceOpt = findType(typeReference.typeName)
    val genericClassReferenceOpts = typeReference.genericTypes.map(getClassReference(_).getOrElse(ClassReference(Core.any, Seq())))
    classReferenceOpt.map { classReference =>
      ClassReference(
        classDefinition = classReference,
        genericTypes = genericClassReferenceOpts
      )
    }
  }
}

case class DefinedFunctionContext(
  outerContext: Context,
  currentFunction: DefinedFunction
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentFunction.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
    outerContext.findType(typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[Verification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findReference(name: String): Option[ElementReference] = {
    currentFunction.parameters
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => getClassReference(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti functions
        }
      }
      .orElse(outerContext.findReference(name))
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

  private def getClassReference(typeReference: TypeReference): Option[ClassReference] = {
    val classReferenceOpt = findType(typeReference.typeName)
    val genericClassReferenceOpts = typeReference.genericTypes.map(getClassReference(_).getOrElse(ClassReference(Core.any, Seq())))
    classReferenceOpt.map { classReference =>
      ClassReference(
        classDefinition = classReference,
        genericTypes = genericClassReferenceOpts
      )
    }
  }
}

case class NamedFunctionReferenceContext(
  outerContext: Context,
  currentFunction: NamedFunction
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    currentFunction.genericTypes.contains(typeName) || outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
    outerContext.findType(typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[Verification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findReference(name: String): Option[ElementReference] = {
    currentFunction.parameters
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => getClassReference(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti functions
        }
      }
      .orElse(outerContext.findReference(name))
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

  private def getClassReference(typeReference: TypeReference): Option[ClassReference] = {
    val classReferenceOpt = findType(typeReference.typeName)
    val genericClassReferenceOpts = typeReference.genericTypes.map(getClassReference(_).getOrElse(ClassReference(Core.any, Seq())))
    classReferenceOpt.map { classReference =>
      ClassReference(
        classDefinition = classReference,
        genericTypes = genericClassReferenceOpts
      )
    }
  }
}

case class LambdaContext(
  outerContext: Context,
  lambda: LambdaExpression
) extends Context {
  override def isTypeAvailable(typeName: String): Boolean = {
    outerContext.isTypeAvailable(typeName)
  }

  override def findType(typeName: String): Option[ClassDefinition] = {
    outerContext.findType(typeName)
  }

  override def isVerificationAvailable(verificationName: String): Boolean = {
    outerContext.isVerificationAvailable(verificationName)
  }

  override def findVerification(verificationName: String): Option[Verification] = {
    outerContext.findVerification(verificationName)
  }

  override def isFunctionAvailable(functionName: String): Boolean = {
    outerContext.isFunctionAvailable(functionName)
  }

  override def findFunction(functionName: String): Option[NamedFunction] = {
    outerContext.findFunction(functionName)
  }

  override def findReference(name: String): Option[ElementReference] = {
    lambda.parameterList
      .find(_.name == name)
      .flatMap {
        _.typeReference match {
          case typeReference: TypeReference => getClassReference(typeReference)
          case _ => None // Currently, lambdaReference are not accepted for definiti functions
        }
      }
      .orElse(outerContext.findReference(name))
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

  private def getClassReference(typeReference: TypeReference): Option[ClassReference] = {
    val classReferenceOpt = findType(typeReference.typeName)
    val genericClassReferenceOpts = typeReference.genericTypes.map(getClassReference(_).getOrElse(ClassReference(Core.any, Seq())))
    classReferenceOpt.map { classReference =>
      ClassReference(
        classDefinition = classReference,
        genericTypes = genericClassReferenceOpts
      )
    }
  }
}