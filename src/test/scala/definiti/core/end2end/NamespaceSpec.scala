package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.Ok
import definiti.common.utils.ASTUtils._
import definiti.core.ProgramResultMatchers._

class NamespaceSpec extends EndToEndSpec {
  import NamespaceSpec._

  "Project.generatePublicAST" should "generate the AST with a namespace 'my' containing namespace 'ns'" in {
    val expected = Ok(validSubnamespace)
    val output = processFile("namespace.subnamespace")
    output should beResult[Root](expected)
  }

  "Project.generatePublicAST" should "generate the AST with a namespace 'my' containing namespaces 'sub' then 'ns'" in {
    val expected = Ok(validSub2namespace)
    val output = processFile("namespace.sub2namespace")
    output should beResult[Root](expected)
  }
}

object NamespaceSpec {
  val validSubnamespaceSrc = "src/test/resources/samples/namespace/subnamespace.def"
  val validSubnamespace = root(namespace("ns", "my.ns",
    AliasType(
      kind = AliasTypeKind.Closed,
      name = "AliasString",
      fullName = "my.ns.AliasString",
      genericTypes = Seq.empty,
      parameters = Seq.empty,
      alias = TypeDeclaration("String", Seq.empty, Seq.empty, Location(validSubnamespaceSrc, 3, 20, 3, 26)),
      verifications = Seq.empty,
      methods = Seq.empty,
      inherited = Seq.empty,
      comment = None,
      location = Location(validSubnamespaceSrc, 3, 1, 3, 26)
    )
  ))

  val validSub2namespaceSrc = "src/test/resources/samples/namespace/sub2namespace.def"
  val validSub2namespace = root(namespace("ns", "my.sub.ns",
    AliasType(
      kind = AliasTypeKind.Closed,
      name = "AliasString",
      fullName = "my.sub.ns.AliasString",
      genericTypes = Seq.empty,
      parameters = Seq.empty,
      alias = TypeDeclaration("String", Seq.empty, Seq.empty, Location(validSub2namespaceSrc, 3, 20, 3, 26)),
      verifications = Seq.empty,
      methods = Seq.empty,
      inherited = Seq.empty,
      comment = None,
      location = Location(validSub2namespaceSrc, 3, 1, 3, 26)
    )
  ))
}
