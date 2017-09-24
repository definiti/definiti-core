package definiti.core.structure

import definiti.core.ast.typed.RootFile
import definiti.core.ast.{pure, structure, typed}

class ProjectStructure(root: typed.Root) {
  def generateStructure(): structure.Root = {
    val filesOfPackage = rootFileWithoutPackage()
    val elementsOfFiles = filesOfPackage.flatMap(packageElementsOfFile)

    val topLevelPackageNames = topLevelPackages()
    val subPackages = topLevelPackageNames.map(buildPackage)

    structure.Root(Seq(elementsOfFiles ++ subPackages: _*))
  }

  private def rootFileWithoutPackage(): Seq[RootFile] = {
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
    topLevelPackages.distinct
  }

  private def buildPackage(packageName: String): structure.Package = {
    val filesOfPackage = extractFileOfPackage(packageName)
    val elementsOfFiles = filesOfPackage.flatMap(packageElementsOfFile)

    val subPackageNames = extractSubPackageNames(packageName)
    val subPackages = subPackageNames.map(buildPackage)

    val packageElements = Seq(elementsOfFiles ++ subPackages: _*)
    structure.Package(packageName, packageElements)
  }

  private def packageElementsOfFile(rootFile: RootFile): Seq[structure.PackageElement] = {
    val verifications = rootFile.verifications.map(transformVerification)
    val classDefinition = rootFile.classDefinitions.flatMap(transformClassDefinition)
    val namedFunctions = rootFile.namedFunctions.map(transformNamedFunction)
    val extendedContexts = rootFile.contexts.map(transformExtendedContext(_))

    Seq(verifications ++ classDefinition ++ namedFunctions ++ extendedContexts: _*)
  }

  private def extractFileOfPackage(packageName: String): Seq[RootFile] = {
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

  private def transformVerification(verification: typed.Verification): structure.Verification = {
    structure.Verification(
      name = verification.name,
      message = verification.message,
      function = verification.function,
      comment = verification.comment,
      range = verification.range
    )
  }

  private def transformClassDefinition(classDefinition: typed.ClassDefinition): Option[structure.ClassDefinition] = {
    classDefinition match {
      case _: typed.NativeClassDefinition => None
      case definedType: typed.DefinedType => Some(transformDefinedType(definedType))
      case aliasType: typed.AliasType => Some(transformAliasType(aliasType))
    }
  }

  private def transformDefinedType(definedType: typed.DefinedType): structure.DefinedType = {
    structure.DefinedType(
      name = definedType.name,
      genericTypes = definedType.genericTypes,
      attributes = definedType.attributes,
      verifications = definedType.verifications,
      inherited = definedType.inherited,
      comment = definedType.comment,
      range = definedType.range
    )
  }

  private def transformAliasType(aliasType: typed.AliasType): structure.AliasType = {
    structure.AliasType(
      name = aliasType.name,
      genericTypes = aliasType.genericTypes,
      alias = aliasType.alias,
      inherited = aliasType.inherited,
      comment = aliasType.comment,
      range = aliasType.range
    )
  }

  private def transformNamedFunction(namedFunction: typed.NamedFunction): structure.NamedFunction = {
    structure.NamedFunction(
      name = namedFunction.name,
      genericTypes = namedFunction.genericTypes,
      parameters = namedFunction.parameters,
      returnType = namedFunction.returnType,
      body = namedFunction.body,
      range = namedFunction.range
    )
  }

  private def transformExtendedContext[A](extendedContext: pure.ExtendedContext[A]): structure.ExtendedContext[A] = {
    structure.ExtendedContext(
      name = extendedContext.name,
      content = extendedContext.content,
      range = extendedContext.range
    )
  }
}
