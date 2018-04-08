package definiti.core.structure

import definiti.core.ast._
import definiti.core.ast.typed.TypedRootFile
import definiti.core.utils.StringUtils

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
    val subPackages = subPackageNames.map(subPackageName => buildPackage(s"${packageName}.${subPackageName}"))

    val packageElements = Seq(elementsOfFiles ++ subPackages: _*)
    Namespace(StringUtils.lastPart(packageName), packageName, packageElements)
  }

  private def packageElementsOfFile(rootFile: TypedRootFile): Seq[NamespaceElement] = {
    val verifications = rootFile.verifications.map(transformVerification(_, rootFile.packageName))
    val classDefinition = rootFile.classDefinitions.flatMap(transformClassDefinition(_, rootFile.packageName))
    val namedFunctions = rootFile.namedFunctions.map(transformNamedFunction(_, rootFile.packageName))
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
    directSubPackageNames.distinct
  }

  private def transformVerification(verification: typed.TypedVerification, namespace: String): Verification = {
    Verification(
      name = verification.name,
      fullName = if (namespace.nonEmpty) namespace + "." + verification.name else verification.name,
      parameters = verification.parameters,
      message = verification.message,
      function = verification.function,
      comment = verification.comment,
      location = verification.location
    )
  }

  private def transformClassDefinition(classDefinition: typed.TypedClassDefinition, namespace: String): Option[ClassDefinition] = {
    classDefinition match {
      case _: typed.TypedNativeClassDefinition => None
      case definedType: typed.TypedDefinedType => Some(transformDefinedType(definedType, namespace))
      case aliasType: typed.TypedAliasType => Some(transformAliasType(aliasType, namespace))
      case enum: typed.TypedEnum => Some(transformEnum(enum, namespace))
    }
  }

  private def transformDefinedType(definedType: typed.TypedDefinedType, namespace: String): DefinedType = {
    DefinedType(
      name = definedType.name,
      fullName = if (namespace.nonEmpty) s"${namespace}.${definedType.name}" else definedType.name,
      genericTypes = definedType.genericTypes,
      parameters = definedType.parameters,
      attributes = definedType.attributes,
      verifications = definedType.verifications,
      inherited = definedType.inherited,
      comment = definedType.comment,
      location = definedType.location
    )
  }

  private def transformAliasType(aliasType: typed.TypedAliasType, namespace: String): AliasType = {
    AliasType(
      name = aliasType.name,
      fullName = if (namespace.nonEmpty) s"${namespace}.${aliasType.name}" else aliasType.name,
      genericTypes = aliasType.genericTypes,
      parameters = aliasType.parameters,
      alias = aliasType.alias,
      verifications = aliasType.verifications,
      inherited = aliasType.inherited,
      comment = aliasType.comment,
      location = aliasType.location
    )
  }

  private def transformEnum(enum: typed.TypedEnum, namespace: String): Enum = {
    Enum(
      name = enum.name,
      fullName = if (namespace.nonEmpty) s"${namespace}.${enum.name}" else enum.name,
      cases = enum.cases.map { enumCase =>
        EnumCase(
          name = enumCase.name,
          comment = enumCase.comment,
          location = enumCase.location
        )
      },
      comment = enum.comment,
      location = enum.location
    )
  }

  private def transformNamedFunction(namedFunction: typed.TypedNamedFunction, namespace: String): NamedFunction = {
    NamedFunction(
      name = namedFunction.name,
      fullName = if (namespace.nonEmpty) s"${namespace}.${namedFunction.name}" else namedFunction.name,
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
