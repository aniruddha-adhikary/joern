package io.joern.x2cpg.frontendspecific.arl2cpg

import io.joern.x2cpg.passes.controlflow.cfgcreation.{Cfg, CfgCreator, CfgEdge}
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Block, CfgNode, Method}
import io.shiftleft.semanticcpg.language.*

class ArlCfgCreator(entryNode: Method, diffGraph: DiffGraphBuilder) extends CfgCreator(entryNode, diffGraph) {

  override protected def cfgFor(node: AstNode): Cfg = node match {
    case block: Block if block.code == "fork" =>
      val branches = block.astChildren.l.collect { case child: Block => child }
      if (branches.size >= 2) cfgForFork(block, branches) else super.cfgFor(node)
    case _ => super.cfgFor(node)
  }

  private def cfgForFork(block: Block, branches: List[Block]): Cfg = {
    val branchCfgs = branches.map { branch =>
      val branchCfg   = cfgFor(branch)
      val childrenCfg =
        branch.astChildren.l.map(cfgFor).reduceOption((accumCfg, nextCfg) => accumCfg ++ nextCfg).getOrElse(Cfg.empty)
      branchCfg.copy(entryNode = childrenCfg.entryNode, edges = childrenCfg.edges, fringe = childrenCfg.fringe)
    }
    val forkEdges = branchCfgs.flatMap(_.entryNode.map(entry => CfgEdge(block, entry, AlwaysEdge)))
    val fringe    = branchCfgs.flatMap { branchCfg =>
      if (branchCfg.entryNode.isDefined) branchCfg.fringe else List((block: CfgNode, AlwaysEdge))
    }

    Cfg
      .from(branchCfgs*)
      .copy(entryNode = Some(block), edges = forkEdges ++ branchCfgs.flatMap(_.edges), fringe = fringe)
  }
}
