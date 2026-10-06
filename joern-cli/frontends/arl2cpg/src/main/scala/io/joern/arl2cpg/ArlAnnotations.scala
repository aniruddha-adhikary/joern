package io.joern.arl2cpg

import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{
  AbstractNode,
  NewAnnotation,
  NewAnnotationLiteral,
  NewAnnotationParameter,
  NewAnnotationParameterAssign
}

object ArlAnnotations {

  def addValue(builder: DiffGraphBuilder, parent: AbstractNode, name: String, value: String, order: Int): Unit = {
    val annotation = NewAnnotation()
      .name(name)
      .fullName(name)
      .code(s"$name: $value")
      .order(order)
    val assignment = NewAnnotationParameterAssign().code(value).order(1)
    val parameter  = NewAnnotationParameter().code("value").order(1)
    val literal    = NewAnnotationLiteral().name(value).code(value).order(2).argumentIndex(2)

    builder.addNode(annotation)
    builder.addNode(assignment)
    builder.addNode(parameter)
    builder.addNode(literal)
    builder.addEdge(parent, annotation, EdgeTypes.AST)
    builder.addEdge(annotation, assignment, EdgeTypes.AST)
    builder.addEdge(assignment, parameter, EdgeTypes.AST)
    builder.addEdge(assignment, literal, EdgeTypes.AST)
  }
}
