package io.joern.arl2cpg.passes

import io.joern.arl2cpg.ArlAnnotations
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
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
        ArlAnnotations.addValue(builder, method, "arlKind", "xom", annotationOrder)
      }
  }
}
