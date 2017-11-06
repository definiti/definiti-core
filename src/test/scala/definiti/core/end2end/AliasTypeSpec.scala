package definiti.core.end2end

import definiti.core.ValidationMatchers.valid
import definiti.core.ast.Root

class AliasTypeSpec extends EndToEndSpec {
  "Project.generatePublicAST" should "generate the AST with an alias containing generics" in {
    val output = processFile("aliasTypes.ListAlias")
    output shouldBe valid[Root]
  }
}