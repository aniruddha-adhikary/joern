package io.joern.arl2cpg.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.codepropertygraph.generated.nodes.NewType
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, Properties}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class MissingTypeNodePass(cpg: Cpg, registered: Option[Set[String]]) extends CpgPass(cpg, "types") {

  private def typeDeclTypes: Set[String] =
    cpg.typeDecl.l.flatMap(typeDecl => typeDecl.fullName :: typeDecl.inheritsFromTypeFullName.toList).toSet

  private def typeFullNamesFromCpg: Set[String] =
    cpg.all.map(_.property(Properties.TypeFullName)).filter(_ != null).toSet

  override def run(builder: DiffGraphBuilder): Unit = {
    val typeFullNameValues = registered.getOrElse(typeFullNamesFromCpg)
    val existing           = cpg.typ.fullName.toSet
    val missingTypes       = (typeDeclTypes ++ typeFullNameValues + Defines.Any)
      .filterNot(_ == "<empty>")
      .filterNot(_.endsWith(NamespaceTraversal.globalNamespaceName))
      .diff(existing)

    missingTypes.toList.sorted.foreach { typeName =>
      builder.addNode(
        NewType()
          .name(TypeNodePass.fullToShortName(typeName))
          .fullName(typeName)
          .typeDeclFullName(typeName)
      )
    }
  }
}
