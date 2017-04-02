package state.api

import state.{AttributeDefinition, NativeClassDefinition, NativeMethodDefinition, ParameterDefinition}

object Core {
  val unit = NativeClassDefinition(
    name = "unit",
    attributes = Seq(),
    methods = Seq()
  )

  val any = NativeClassDefinition(
    name = "any",
    attributes = Seq(),
    methods = Seq()
  )

  val string = NativeClassDefinition(
    name = "String",
    attributes = Seq(),
    methods = Seq(
      NativeMethodDefinition(
        name = "nonEmpty",
        parameters = Seq(),
        returnTypeReference = "Boolean"
      ),
      NativeMethodDefinition(
        name = "trim",
        parameters = Seq(),
        returnTypeReference = "String"
      ),
      NativeMethodDefinition(
        name = "startsWith",
        parameters = Seq(
          ParameterDefinition("prefix","String")
        ),
        returnTypeReference = "Boolean"
      ),
      NativeMethodDefinition(
        name = "matches",
        parameters = Seq(
          ParameterDefinition("regex", "String")
        ),
        returnTypeReference = "Boolean"
      )
    )
  )

  val date = NativeClassDefinition(
    name = "Date",
    attributes = Seq(
      AttributeDefinition("timestamp", "Number"),
      AttributeDefinition("day", "Number"),
      AttributeDefinition("month", "Number")
    ),
    methods = Seq()
  )

  val boolean = NativeClassDefinition(
    name = "Boolean",
    attributes = Seq(),
    methods = Seq()
  )

  val number = NativeClassDefinition(
    name = "Number",
    attributes = Seq(),
    methods = Seq()
  )
}
