package definiti.common.ast

sealed trait AbstractTypeReference {
  def readableString: String
}

case object Unset extends AbstractTypeReference {
  override def readableString: String = "***Unset***"
}

case class TypeReference(
  typeName: String,
  genericTypes: Seq[TypeReference] = Seq.empty
) extends AbstractTypeReference {
  def readableString: String = {
    if (genericTypes.nonEmpty) {
      s"$typeName[${genericTypes.map(_.readableString).mkString(",")}]"
    } else {
      typeName
    }
  }
}

case class LambdaReference(
  inputTypes: Seq[TypeReference],
  outputType: TypeReference
) extends AbstractTypeReference {
  def readableString: String = s"(${inputTypes.map(_.readableString).mkString(", ")}) => ${outputType.readableString}"
}

case class NamedFunctionReference(
  functionName: String
) extends AbstractTypeReference {
  def readableString: String = functionName
}

case class TypeDeclaration(
  typeName: String,
  genericTypes: Seq[TypeDeclaration],
  parameters: Seq[AtomicExpression],
  location: Location
) {
  def readableString: String = {
    if (genericTypes.nonEmpty) {
      s"$typeName[${genericTypes.map(_.readableString).mkString(", ")}]"
    } else {
      typeName
    }
  }
}