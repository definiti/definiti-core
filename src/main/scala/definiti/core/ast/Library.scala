package definiti.core.ast

import scala.collection.mutable.ListBuffer

case class Library(
  packages: Map[String, Namespace],
  verifications: Map[String, Verification],
  types: Map[String, ClassDefinition],
  attributes: Map[String, AttributeDefinition],
  methods: Map[String, MethodDefinition],
  namedFunctions: Map[String, NamedFunction]
)

object Library {
  def apply(root: Root, core: Seq[ClassDefinition]): Library = {
    val packages = extractPackages(root)
    val types = extractTypes(packages) ++ extractCore(core)
    new Library(
      packages = packages,
      verifications = extractVerifications(packages),
      types = types,
      attributes = extractAttributes(types),
      methods = extractMethods(types),
      namedFunctions = extractNamedFunctions(packages)
    )
  }

  private def extractPackages(root: Root): Map[String, Namespace] = {
    val packagesBuffer: ListBuffer[(String, Namespace)] = ListBuffer()

    def extractFromPackage(thePackage: Namespace, packageName: String): Unit = {
      packagesBuffer += (packageName -> thePackage)
      thePackage.elements.foreach {
        case subPackage: Namespace => extractFromPackage(subPackage, s"${packageName}.${subPackage.name}")
        case _ => // do nothing
      }
    }

    root.elements.foreach {
      case thePackage: Namespace => extractFromPackage(thePackage, thePackage.name)
      case _ => // do nothing
    }

    packagesBuffer.toMap
  }

  private def extractVerifications(packages: Map[String, Namespace]): Map[String, Verification] = {
    packages.flatMap { case (packageName, thePackage) =>
      thePackage.elements.collect {
        case verification: Verification => s"${packageName}.${verification.name}" -> verification
      }
    }
  }

  private def extractTypes(packages: Map[String, Namespace]): Map[String, ClassDefinition] = {
    packages.flatMap { case (packageName, thePackage) =>
      thePackage.elements.collect {
        case theType: ClassDefinition => s"${packageName}.${theType.name}" -> theType
      }
    }
  }

  private def extractAttributes(types: Map[String, ClassDefinition]): Map[String, AttributeDefinition] = {
    def extractAttributesFromClass(classDefinition: ClassDefinition): Seq[AttributeDefinition] = {
      classDefinition match {
        case nativeClassDefinition: NativeClassDefinition => nativeClassDefinition.attributes
        case definedType: DefinedType => definedType.attributes
        case aliasType: AliasType => extractAttributesFromClass(types(aliasType.alias.typeName))
      }
    }

    types.flatMap { case (typeName, theType) =>
      extractAttributesFromClass(theType)
        .map(attribute => s"${typeName}.${attribute.name}" -> attribute)
    }
  }

  private def extractMethods(types: Map[String, ClassDefinition]): Map[String, MethodDefinition] = {
    def extractMethodsFromClass(classDefinition: ClassDefinition): Seq[MethodDefinition] = {
      classDefinition match {
        case nativeClassDefinition: NativeClassDefinition => nativeClassDefinition.methods
        case _: DefinedType => Seq.empty
        case aliasType: AliasType => extractMethodsFromClass(types(aliasType.alias.typeName))
      }
    }

    types.flatMap { case (typeName, theType) =>
      extractMethodsFromClass(theType)
        .map(method => s"${typeName}.${method.name}" -> method)
    }
  }

  private def extractNamedFunctions(packages: Map[String, Namespace]): Map[String, NamedFunction] = {
    packages.flatMap { case (packageName, thePackage) =>
      thePackage.elements.collect {
        case namedFunction: NamedFunction => s"${packageName}.${namedFunction.name}" -> namedFunction
      }
    }
  }

  private def extractCore(core: Seq[ClassDefinition]): Map[String, ClassDefinition] = {
    core
      .map(classDefinition => classDefinition.name -> classDefinition)
      .toMap
  }
}