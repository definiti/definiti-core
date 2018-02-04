package definiti.core.ast

import scala.collection.mutable.ListBuffer

case class Library(root: Root, core: Seq[ClassDefinition]) {
  lazy val namespacesMap: Map[String, Namespace] = namespaces.map(n => n.fullName -> n).toMap
  lazy val verificationsMap: Map[String, Verification] = verifications.map(v => v.fullName -> v).toMap
  lazy val typesMap: Map[String, ClassDefinition] = types.map(t => t.fullName -> t).toMap
  lazy val namedFunctionsMap: Map[String, NamedFunction] = namedFunctions.map(n => n.fullName -> n).toMap

  lazy val namespaces: Seq[Namespace] = Library.extractPackages(root)
  lazy val verifications: Seq[Verification] = fromNamespaces { case verification: Verification => verification }
  lazy val types: Seq[ClassDefinition] = projectTypes ++ core
  lazy val projectTypes: Seq[ProjectClassDefinition] = aliasTypes ++ definedTypes ++ enums
  lazy val aliasTypes: Seq[AliasType] = fromNamespaces { case aliasType: AliasType => aliasType }
  lazy val definedTypes: Seq[DefinedType] = fromNamespaces { case definedType: DefinedType => definedType }
  lazy val enums: Seq[Enum] = fromNamespaces { case enum: Enum => enum }
  lazy val namedFunctions: Seq[NamedFunction] = fromNamespaces { case namedFunction: NamedFunction => namedFunction }

  private def fromNamespaces[A](extractor: PartialFunction[NamespaceElement, A]): Seq[A] = {
    root.elements.collect(extractor) ++ namespaces.flatMap(_.elements.collect(extractor))
  }
}

object Library {
  def extractPackages(root: Root): Seq[Namespace] = {
    val namespacesBuffer: ListBuffer[Namespace] = ListBuffer()

    def process(namespace: Namespace): Unit = {
      namespacesBuffer += namespace
      namespace.elements.foreach {
        case subNamespace: Namespace => process(subNamespace)
        case _ => // do nothing
      }
    }

    root.elements.foreach {
      case namespace: Namespace => process(namespace)
      case _ => // do nothing
    }

    namespacesBuffer.toList
  }
}