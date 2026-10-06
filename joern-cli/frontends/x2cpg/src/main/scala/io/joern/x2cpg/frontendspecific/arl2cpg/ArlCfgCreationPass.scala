package io.joern.x2cpg.frontendspecific.arl2cpg

import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class ArlCfgCreationPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    new ArlCfgCreator(method, diffGraph).run()
  }
}
