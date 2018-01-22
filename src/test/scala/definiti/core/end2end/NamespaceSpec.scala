package definiti.core.end2end

import definiti.core.ValidValue
import definiti.core.ValidationMatchers.beValidated
import definiti.core.ast._

class NamespaceSpec extends EndToEndSpec {
  import NamespaceSpec._

  "Project.generatePublicAST" should "generate the AST with a namespace 'my' containing namespace 'ns'" in {
    val expected = ValidValue(validSubnamespace)
    val output = processFile("namespace.subnamespace")
    output should beValidated[Root](expected)
  }

  "Project.generatePublicAST" should "generate the AST with a namespace 'my' containing namespaces 'sub' then 'ns'" in {
    val expected = ValidValue(validSub2namespace)
    val output = processFile("namespace.sub2namespace")
    output should beValidated[Root](expected)
  }
}

object NamespaceSpec {
  val validSubnamespaceSrc = "src/test/resources/samples/namespace/subnamespace.def"
  val validSubnamespace = Root(Seq(
    Namespace(
      name = "my",
      fullName = "my",
      elements = Seq(
        Namespace(
          name = "ns",
          fullName = "my.ns",
          elements = Seq(AliasType(
            name = "AliasString",
            fullName = "my.ns.AliasString",
            genericTypes = Seq.empty,
            alias = TypeReference("String"),
            verifications = Seq.empty,
            inherited = Seq.empty,
            comment = None,
            location = Location(validSubnamespaceSrc, 3, 1, 3, 26)
          ))
        )
      )
    )
  ))

  val validSub2namespaceSrc = "src/test/resources/samples/namespace/sub2namespace.def"
  val validSub2namespace = Root(Seq(
    Namespace(
      name = "my",
      fullName = "my",
      elements = Seq(
        Namespace(
          name = "sub",
          fullName = "my.sub",
          elements = Seq(
            Namespace(
              name = "ns",
              fullName = "my.sub.ns",
              elements = Seq(AliasType(
                name = "AliasString",
                fullName = "my.sub.ns.AliasString",
                genericTypes = Seq.empty,
                alias = TypeReference("String"),
                verifications = Seq.empty,
                inherited = Seq.empty,
                comment = None,
                location = Location(validSub2namespaceSrc, 3, 1, 3, 26)
              ))
            )
          )
        )
      )
    )
  ))
}
