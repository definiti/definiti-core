package definiti.common.utils

import definiti.common.ast._

object ASTUtils {
  def root(namespaces: Namespace*): Root = {
    Root(namespaces)
  }

  def root(namespaceElements: NamespaceElement*)(implicit dummyImplicit: DummyImplicit): Root = {
    Root(Seq(Namespace("", "", namespaceElements)))
  }

  def namespace(name: String, fullName: String, elements: NamespaceElement*): Namespace = {
    Namespace(name, fullName, elements)
  }

  def namespace(fullName: String, elements: NamespaceElement*): Namespace = {
    Namespace(StringUtils.lastPart(fullName), fullName, elements)
  }

  def definedType(
    fullName: String,
    location: Location,
    genericTypes: Seq[String] = Seq.empty,
    parameters: Seq[ParameterDefinition] = Seq.empty,
    attributes: Seq[AttributeDefinition] = Seq.empty,
    verifications: Seq[TypeVerification] = Seq.empty,
    inherited: Seq[VerificationReference] = Seq.empty,
    comment: Option[String] = None
  ): DefinedType = {
    DefinedType(
      name = StringUtils.lastPart(fullName),
      fullName = fullName,
      genericTypes = genericTypes,
      parameters = parameters,
      attributes = attributes,
      verifications = verifications,
      inherited = inherited,
      comment = comment,
      location = location
    )
  }

  def attributeDefinition(
    name: String,
    typeDeclaration: TypeDeclaration,
    location: Location,
    comment: Option[String] = None,
    verifications: Seq[VerificationReference] = Seq.empty,
    typeName: Option[String] = None
  ): AttributeDefinition = {
    AttributeDefinition(
      name = name,
      typeDeclaration = typeDeclaration,
      comment = comment,
      verifications = verifications,
      typeName = typeName,
      location = location
    )
  }

  def typeDeclaration(
    typeName: String,
    location: Location,
    genericTypes: Seq[TypeDeclaration] = Seq.empty,
    parameters: Seq[AtomicExpression] = Seq.empty
  ): TypeDeclaration = {
    TypeDeclaration(
      typeName = typeName,
      genericTypes = genericTypes,
      parameters = parameters,
      location = location
    )
  }
}
