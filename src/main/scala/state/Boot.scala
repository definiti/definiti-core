package state

import state.generators.ScalaGenerator

object Boot extends App {
  val syntax = SyntaxProcessor.processString(Sample.testString)
  val syntaxEnhanced = SyntaxEnhancer.enhanceSyntax(syntax)
  val ast = SyntaxToAstProcessor.transform(syntaxEnhanced)
  SyntaxToAstProcessor.injectIntoReference(ast)
  SyntaxToAstProcessor.validate(ast)

  println(ScalaGenerator.generate(ast))
}