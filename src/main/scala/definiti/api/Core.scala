package definiti.api

import definiti.{AttributeDefinition, NativeClassDefinition, NativeMethodDefinition, ParameterDefinition}

object Core {
  def injectCore(): Unit = {
    TypeReference.referenceType(unit)
    TypeReference.referenceType(any)
    TypeReference.referenceType(string)
    TypeReference.referenceType(date)
    TypeReference.referenceType(boolean)
    TypeReference.referenceType(number)
  }

  val unit = NativeClassDefinition(
    name = "unit",
    attributes = Seq(),
    methods = Seq(),
    None
  )

  val any = NativeClassDefinition(
    name = "any",
    attributes = Seq(),
    methods = Seq(),
    None
  )

  val string = NativeClassDefinition(
    name = "String",
    attributes = Seq(),
    methods = Seq(
      NativeMethodDefinition(
        name = "nonEmpty",
        parameters = Seq(),
        returnTypeReference = "Boolean",
        None
      ),
      NativeMethodDefinition(
        name = "trim",
        parameters = Seq(),
        returnTypeReference = "String",
        None
      ),
      NativeMethodDefinition(
        name = "startsWith",
        parameters = Seq(
          ParameterDefinition("prefix","String")
        ),
        returnTypeReference = "Boolean",
        None
      ),
      NativeMethodDefinition(
        name = "matches",
        parameters = Seq(
          ParameterDefinition("regex", "String")
        ),
        returnTypeReference = "Boolean",
        None
      )
    ),
    None
  )

  val date = NativeClassDefinition(
    name = "Date",
    attributes = Seq(
      AttributeDefinition("timestamp", "Number",
        None),
      AttributeDefinition("day", "Number",
        None),
      AttributeDefinition("month", "Number",
        None)
    ),
    methods = Seq(),
    None
  )

  val boolean = NativeClassDefinition(
    name = "Boolean",
    attributes = Seq(),
    methods = Seq(),
    None
  )

  val number = NativeClassDefinition(
    name = "Number",
    attributes = Seq(),
    methods = Seq(),
    None
  )
}
