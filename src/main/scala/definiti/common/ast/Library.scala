package definiti.common.ast

case class Library(root: Root, core: Seq[ClassDefinition]) {
  lazy val namespacesMap: Map[String, Namespace] = namespaces.map(n => n.fullName -> n).toMap
  lazy val verificationsMap: Map[String, Verification] = verifications.map(v => v.fullName -> v).toMap
  lazy val typesMap: Map[String, ClassDefinition] = buildTypeMaps()
  lazy val namedFunctionsMap: Map[String, NamedFunction] = namedFunctions.map(n => n.fullName -> n).toMap

  lazy val namespaces: Seq[Namespace] = root.namespaces
  lazy val verifications: Seq[Verification] = fromNamespaces { case verification: Verification => verification }
  lazy val types: Seq[ClassDefinition] = projectTypes ++ core
  lazy val projectTypes: Seq[ProjectClassDefinition] = aliasTypes ++ definedTypes ++ enums
  lazy val aliasTypes: Seq[AliasType] = fromNamespaces { case aliasType: AliasType => aliasType }
  lazy val definedTypes: Seq[DefinedType] = fromNamespaces { case definedType: DefinedType => definedType }
  lazy val enums: Seq[Enum] = fromNamespaces { case enum: Enum => enum }
  lazy val namedFunctions: Seq[NamedFunction] = fromNamespaces { case namedFunction: NamedFunction => namedFunction }

  private def buildTypeMaps(): Map[String, ClassDefinition] = {
    val mainTypes = types.map(t => t.fullName -> t).toMap
    val attributeTypes = for {
      definedType <- definedTypes
      attribute <- definedType.attributes
      typeName <- attribute.typeName.toSeq
    } yield {
      val fullTypeName = s"${definedType.fullName}.${typeName}"
      val classDefinition = AliasType(
        name = s"${definedType.name}.${typeName}",
        fullName = fullTypeName,
        genericTypes = Seq.empty,
        parameters = Seq.empty,
        alias = attribute.typeDeclaration,
        inherited = attribute.verifications,
        verifications = Seq.empty,
        comment = attribute.comment,
        location = attribute.location
      )
      fullTypeName -> classDefinition
    }

    mainTypes ++ attributeTypes.toMap
  }

  private def fromNamespaces[A](extractor: PartialFunction[NamespaceElement, A]): Seq[A] = {
    root.namespaces.flatMap(_.elements).collect(extractor)
  }
}