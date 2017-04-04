package definiti

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import definiti.api.{DefinitionParser, TypeReference}
import definiti.generators.{ScalaGenerator, TypescriptGenerator}

import scala.collection.JavaConverters._

object Boot extends App {
  val source = Paths.get("src", "main", "resources", "samples", "first.def")
  val destination = Map(
    "scala" -> Paths.get("target", "samples", "result.scala"),
    "typescript" -> Paths.get("target", "samples", "result.typescript")
  )
  val rootDefinitionFiles = Paths.get("src", "main", "resources", "api")
  val definitionFiles = Files.find(rootDefinitionFiles, 1000, (path, _) => String.valueOf(path).endsWith(".definition"))

  val stringSource = Files.readAllLines(source).asScala.mkString("\n")
  val syntax = SyntaxProcessor.processString(stringSource)
  val syntaxEnhanced = SyntaxEnhancer.enhanceSyntax(syntax)
  val ast = SyntaxToAstProcessor.transform(syntaxEnhanced)

  definitionFiles.forEach { definitionFile =>
    val classDefinitions = DefinitionParser.parse(Files.readAllLines(definitionFile).asScala.mkString("", "\n", "\n"))
    classDefinitions.foreach(TypeReference.referenceType)
  }
  SyntaxToAstProcessor.injectIntoReference(ast)
  SyntaxToAstProcessor.validate(ast)

  destination.foreach { case (language, path) =>
    def write(path: Path, str: String): Unit = {
      Files.createDirectories(path.getParent)
      Files.write(path, Seq(str).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    }

    language match {
      case "scala" => write(path, ScalaGenerator.generate(ast))
      case "typescript" => write(path, TypescriptGenerator.generate(ast))
    }
  }
}