package state

import spray.json._

object Boot extends App {
  import ASTJsonProtocol._

  val syntax = SyntaxProcessor.processString(Sample.testString)
  val syntaxEnhanced = SyntaxEnhancer.enhanceSyntax(syntax)
  val ast = SyntaxToAstProcessor.transform(syntaxEnhanced)
  SyntaxToAstProcessor.injectIntoReference(ast)
  SyntaxToAstProcessor.validate(ast)

  println(ast.toJson.prettyPrint)
}