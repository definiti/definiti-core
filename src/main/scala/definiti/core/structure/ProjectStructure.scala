package definiti.core.structure

import definiti.core.ast.typed.TypedRootFile
import definiti.core.ast._

private[core] class ProjectStructure(root: typed.TypedRoot) {
  def generateStructure(): Root = {
    val filesOfPackage = rootFileWithoutPackage()
    val elementsOfFiles = filesOfPackage.flatMap(packageElementsOfFile)

    val topLevelPackageNames = topLevelPackages()
    val subPackages = topLevelPackageNames.map(buildPackage)

    Root(Seq(elementsOfFiles ++ subPackages: _*))
  }

  private def rootFileWithoutPackage(): Seq[TypedRootFile] = {
    root.files.filter(_.packageName.isEmpty)
  }

  private def topLevelPackages(): Seq[String] = {
    val topLevelPackages = root.files.map { rootFile =>
      val packageName = rootFile.packageName
      if (packageName.contains(".")) {
        packageName.substring(0, packageName.indexOf("."))
      } else {
        packageName
      }
    }
    topLevelPackages.filter(_.nonEmpty).distinct
  }

  private def buildPackage(packageName: String): Namespace = {
    val filesOfPackage = extractFileOfPackage(packageName)
    val elementsOfFiles = filesOfPackage.flatMap(packageElementsOfFile)

    val subPackageNames = extractSubPackageNames(packageName)
    val subPackages = subPackageNames.map(buildPackage)

    val packageElements = Seq(elementsOfFiles ++ subPackages: _*)
    Namespace(packageName, packageElements)
  }

  private def packageElementsOfFile(rootFile: TypedRootFile): Seq[NamespaceElement] = {
    val verifications = rootFile.verifications.map(transformVerification)
    val classDefinition = rootFile.classDefinitions.flatMap(transformClassDefinition)
    val namedFunctions = rootFile.namedFunctions.map(transformNamedFunction)
    val extendedContexts = rootFile.contexts.map(transformExtendedContext(_))

    Seq(verifications ++ classDefinition ++ namedFunctions ++ extendedContexts: _*)
  }

  private def extractFileOfPackage(packageName: String): Seq[TypedRootFile] = {
    root.files.filter(_.packageName == packageName)
  }

  private def extractSubPackageNames(packageName: String): Seq[String] = {
    val packageNames = root.files.map(_.packageName)
    val subPackageNames =
      packageNames
        .filter(_.startsWith(s"${packageName}."))
        .map(_.substring(s"${packageName}.".length))
    val directSubPackageNames = subPackageNames.map { packageName =>
      if (packageName.contains(".")) {
        packageName.substring(0, packageName.indexOf("."))
      } else {
        packageName
      }
    }
    directSubPackageNames
  }

  private def transformVerification(verification: typed.TypedVerification): Verification = {
    Verification(
      name = verification.name,
      message = verification.message,
      function = verification.function,
      comment = verification.comment,
      location = verification.location
    )
  }

  private def transformClassDefinition(classDefinition: typed.TypedClassDefinition): Option[ClassDefinition] = {
    classDefinition match {
      case _: typed.TypedNativeClassDefinition => None
      case definedType: typed.TypedDefinedType => Some(transformDefinedType(definedType))
      case aliasType: typed.TypedAliasType => Some(transformAliasType(aliasType))
    }
  }

  private def transformDefinedType(definedType: typed.TypedDefinedType): DefinedType = {
    DefinedType(
      name = definedType.name,
      genericTypes = definedType.genericTypes,
      attributes = definedType.attributes,
      verifications = definedType.verifications,
      inherited = definedType.inherited,
      comment = definedType.comment,
      location = definedType.location
    )
  }

  private def transformAliasType(aliasType: typed.TypedAliasType): AliasType = {
    AliasType(
      name = aliasType.name,
      genericTypes = aliasType.genericTypes,
      alias = aliasType.alias,
      inherited = aliasType.inherited,
      comment = aliasType.comment,
      location = aliasType.location
    )
  }

  private def transformNamedFunction(namedFunction: typed.TypedNamedFunction): NamedFunction = {
    NamedFunction(
      name = namedFunction.name,
      genericTypes = namedFunction.genericTypes,
      parameters = namedFunction.parameters,
      returnType = namedFunction.returnType,
      body = namedFunction.body,
      location = namedFunction.location
    )
  }

  private def transformExtendedContext[A](extendedContext: pure.PureExtendedContext[A]): ExtendedContext[A] = {
    ExtendedContext(
      name = extendedContext.name,
      content = extendedContext.content,
      location = extendedContext.location
    )
  }
}
