package definiti.core.parser.project

import definiti.core.Configuration
import definiti.core.ast.pure._
import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.StringUtils

import scala.collection.mutable.ListBuffer

private[core] class DefinitiASTParser(sourceFile: String, configuration: Configuration) extends CommonParser {
  val file: String = sourceFile.replaceAllLiterally("\\", "/")

  def definitiContextToAST(context: DefinitiContext): PureRootFile = {
    val packageName = extractPackageName(context)
    new DefinitiFileASTParser(
      sourceFile = sourceFile,
      configuration = configuration,
      packageName = packageName,
      imports = extractImports(context) ++ extractImportsForCurrentFile(context, packageName)
    ).parse(context)
  }

  private def extractPackageName(context: DefinitiContext): String = {
    Option(context.packageName())
      .map(packageNameContext => dottedIdentifierToIdentifier(packageNameContext.dottedIdentifier()))
      .getOrElse("")
  }

  private def extractImports(context: DefinitiContext): Map[String, String] = {
    scalaSeq(context.imports())
      .view
      .map(importContext => dottedIdentifierToIdentifier(importContext.dottedIdentifier()))
      .map(fullName => StringUtils.lastPart(fullName) -> fullName)
      .toMap
  }

  private def dottedIdentifierToIdentifier(context: DottedIdentifierContext): String = {
    scalaSeq(context.IDENTIFIER()).map(_.getText).mkString(".")
  }

  private def extractImportsForCurrentFile(context: DefinitiContext, packageName: String): Map[String, String] = {
    extractTopLevelNames(context)
      .map { topLevelName =>
        if (packageName.nonEmpty) {
          topLevelName -> (packageName + "." + topLevelName)
        } else {
          topLevelName -> topLevelName
        }
      }
      .toMap
  }

  private def extractTopLevelNames(context: DefinitiContext): Seq[String] = {
    val topLevelNames = ListBuffer[String]()
    scalaSeq(context.toplevel()).foreach { element =>
      appendIfDefined(element.verification(), topLevelNames, (c: VerificationContext) => c.verificationName.getText)
      appendIfDefined(element.definedType(), topLevelNames, (c: DefinedTypeContext) => c.typeName.getText)
      appendIfDefined(element.aliasType(), topLevelNames, (c: AliasTypeContext) => c.typeName.getText)
      appendIfDefined(element.enumType(), topLevelNames, (c: EnumTypeContext) => c.typeName.getText)
      appendIfDefined(element.namedFunction(), topLevelNames, (c: NamedFunctionContext) => c.name.getText)
    }
    List(topLevelNames: _*)
  }
}
