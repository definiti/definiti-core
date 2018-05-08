package definiti.common.utils

import definiti.common.ast._

object ASTUtils {
  def root(namespaces: Namespace*): Root = {
    Root(namespaces)
  }

  def root(namespaceElements: NamespaceElement*)(implicit dummyImplicit: DummyImplicit): Root = {
    Root(Seq(Namespace("", "", namespaceElements)))
  }

  def namespace(name: String, fullName: String, elements: NamespaceElement*): Namespace = {
    Namespace(name, fullName, elements)
  }
}
