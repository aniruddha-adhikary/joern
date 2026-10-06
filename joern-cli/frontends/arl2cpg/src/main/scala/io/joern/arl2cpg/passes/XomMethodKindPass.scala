package io.joern.arl2cpg.passes

import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

class XomMethodKindPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.method.l
      .filterNot(_.isExternal)
      .filterNot(_.filename.endsWith(".arl"))
      .filter(_.annotation.name("arlKind").l.isEmpty)
      .foreach { method =>
        val annotationOrder = method.astChildren.l.map(_.order).maxOption.getOrElse(0) + 1
        val annotation      = NewAnnotation()
          .name("arlKind")
          .fullName("arlKind")
          .code("arlKind: xom")
          .order(annotationOrder)
        val assignment = NewAnnotationParameterAssign().code("xom").order(1)
        val parameter  = NewAnnotationParameter().code("value").order(1)
        val literal    = NewAnnotationLiteral().name("xom").code("xom").order(2).argumentIndex(2)

        builder.addNode(annotation)
        builder.addNode(assignment)
        builder.addNode(parameter)
        builder.addNode(literal)
        builder.addEdge(method, annotation, EdgeTypes.AST)
        builder.addEdge(annotation, assignment, EdgeTypes.AST)
        builder.addEdge(assignment, parameter, EdgeTypes.AST)
        builder.addEdge(assignment, literal, EdgeTypes.AST)
      }
  }
}
