package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.Ok
import definiti.common.tests.LocationPath
import definiti.common.utils.ASTUtils._
import definiti.core.ProgramResultMatchers._

class AttributeTypeSpec extends EndToEndSpec {

  import AttributeTypeSpec._

  "Project.generatePublicAST" should "generate the AST with an attribute type and its references" in {
    val output = processDirectory("attributeType")
    val expected = Ok(attributeType)
    output should beResult[Root](expected)
  }
}

object AttributeTypeSpec {
  val carLocation = LocationPath("src/test/resources/samples/attributeType/car.def")
  val houseLocation = LocationPath("src/test/resources/samples/attributeType/house.def")
  val userLocation = LocationPath("src/test/resources/samples/attributeType/user.def")
  val attributeType: Root = root(
    namespace(
      "car",
      definedType(
        fullName = "car.Car",
        attributes = Seq(
          attributeDefinition(
            name = "name",
            typeDeclaration = typeDeclaration("String", carLocation(6, 9, 15)),
            location = carLocation(6, 3, 15)
          ),
          attributeDefinition(
            name = "userId",
            typeDeclaration = typeDeclaration("user.User.Id", carLocation(7, 11, 13)),
            location = carLocation(7, 3, 13)
          )
        ),
        location = carLocation(5, 1, 8, 2)
      )
    ),
    namespace(
      "house",
      definedType(
        fullName = "house.House",
        attributes = Seq(
          attributeDefinition(
            name = "name",
            typeDeclaration = typeDeclaration("String", houseLocation(6, 9, 15)),
            location = houseLocation(6, 3, 15)
          ),
          attributeDefinition(
            name = "userId",
            typeDeclaration = typeDeclaration("user.User.Id", houseLocation(7, 11, 18)),
            location = houseLocation(7, 3, 18)
          )
        ),
        location = houseLocation(5, 1, 8, 2)
      )
    ),
    namespace(
      "user",
      definedType(
        fullName = "user.User",
        attributes = Seq(
          attributeDefinition(
            name = "id",
            typeDeclaration = typeDeclaration("String", userLocation(4, 7, 13)),
            attributeType = Some(AttributeType(AliasTypeKind.Closed, "Id")),
            location = userLocation(4, 3, 19)
          )
        ),
        location = userLocation(3, 1, 5, 2)
      )
    )
  )
}
