package definiti

import definiti.api.DefinitionParser

import scala.io.Source

object Boot extends App {
  val syntax = SyntaxProcessor.processString(Sample.testString)
  val syntaxEnhanced = SyntaxEnhancer.enhanceSyntax(syntax)
  val ast = SyntaxToAstProcessor.transform(syntaxEnhanced)
  SyntaxToAstProcessor.injectIntoReference(ast)
  SyntaxToAstProcessor.validate(ast)

  //println(ScalaGenerator.generate(ast))
  //println(TypescriptGenerator.generate(ast))

  println(DefinitionParser.parse(Source.fromResource(s"api/Date.definition").getLines.mkString("", "\n", "\n")))
  println(DefinitionParser.parse(Source.fromResource(s"api/String.definition").getLines.mkString("", "\n", "\n")))
}