package definiti.common.ast

case class Verification(
  name: String,
  fullName: String,
  parameters: Seq[ParameterDefinition],
  message: VerificationMessage,
  function: DefinedFunction,
  comment: Option[String],
  location: Location
) extends NamespaceElement

sealed trait VerificationMessage {
  def prettyPrint: String
}

case class LiteralMessage(message: String, location: Location) extends VerificationMessage {
  override def prettyPrint: String = message
}

case class TypedMessage(message: String, types: Seq[TypeReference], location: Location) extends VerificationMessage {
  override def prettyPrint: String = {
    if (types.nonEmpty) {
      s"""message("${message}", ${types.map(_.readableString).mkString(", ")})"""
    } else {
      s"""message("${message}")"""
    }

  }
}

case class DefinedFunction(parameters: Seq[ParameterDefinition], body: Expression, genericTypes: Seq[String], location: Location)