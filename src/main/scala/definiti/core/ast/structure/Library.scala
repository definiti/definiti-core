package definiti.core.ast.structure

import definiti.core.ast.pure.{AttributeDefinition, MethodDefinition}

import scala.collection.mutable.ListBuffer

case class Library(
  packages: Map[String, Package],
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

  private def extractPackages(root: Root): Map[String, Package] = {
    val packagesBuffer: ListBuffer[(String, Package)] = ListBuffer()

    def extractFromPackage(thePackage: Package, packageName: String): Unit = {
      packagesBuffer += (packageName -> thePackage)
      thePackage.elements.foreach {
        case subPackage: Package => extractFromPackage(subPackage, s"${packageName}.${subPackage.name}")
        case _ => // do nothing
      }
    }

    root.elements.foreach {
      case thePackage: Package => extractFromPackage(thePackage, thePackage.name)
      case _ => // do nothing
    }

    packagesBuffer.toMap
  }

  private def extractVerifications(packages: Map[String, Package]): Map[String, Verification] = {
    packages.flatMap { case (packageName, thePackage) =>
      thePackage.elements.collect {
        case verification: Verification => s"${packageName}.${verification.name}" -> verification
      }
    }
  }

  private def extractTypes(packages: Map[String, Package]): Map[String, ClassDefinition] = {
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

  private def extractNamedFunctions(packages: Map[String, Package]): Map[String, NamedFunction] = {
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