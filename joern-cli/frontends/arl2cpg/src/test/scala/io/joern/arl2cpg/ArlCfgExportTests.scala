package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlExport
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, CfgNode, JumpTarget, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.Files
import scala.io.Source
import scala.util.Using

class ArlCfgExportTests extends Arl2CpgSuite() {

  private val cfgResources = Seq("branch", "forkjoin", "loop", "subflow", "select", "prio", "modes")

  private def resource(name: String): String =
    Using.resource(Source.fromResource(s"arl/$name.arl"))(_.mkString)

  private def withResourceCpg(names: Seq[String])(f: Cpg => Unit): Unit =
    FileUtil.usingTemporaryDirectory("arl2cpg-cfg-export") { dir =>
      names.foreach(name => Files.writeString(dir.resolve(s"$name.arl"), resource(name)))
      val cpg = new Arl2Cpg().createCpg(Config().withInputPath(dir.toString)).get
      try {
        X2Cpg.applyDefaultOverlays(cpg)
        f(cpg)
      } finally {
        cpg.close()
      }
    }

  private def methodIn(cpg: Cpg, name: String, file: String): Method =
    cpg.method
      .nameExact(name)
      .l
      .find(method =>
        method.filename.endsWith(s"/$file.arl") &&
          method.annotation.name("arlKind").parameterAssign.value.code.headOption.contains("flowtask")
      )
      .get

  private def taskCall(method: Method, name: String): Call =
    method.call.l.find(_.name == name).get

  private def blockWithCode(method: Method, code: String): Block =
    method.ast.l.collect { case block: Block if block.code == code => block }.head

  private def successorCodes(node: CfgNode): Set[String] =
    node.cfgNext.l.map(_.code.trim).toSet

  "ARL CFG construction" should {
    "branch, fork, loop, subflow, select, priority, and modes according to their lowered flow order" in {
      withResourceCpg(cfgResources) { cpg =>
        val branch         = methodIn(cpg, "probe branch", "branch")
        val branchIf       = branch.controlStructure.l.find(_.code.trim.startsWith("if")).get
        val thenCall       = taskCall(branch, "probe branch>hi")
        val elseCall       = taskCall(branch, "probe branch>lo")
        val conditionNodes =
          (branchIf.condition.l ++ branchIf.condition.ast.l).collect { case node: CfgNode => node }
        val branchEntries       = Set(thenCall.id, elseCall.id)
        val conditionSuccessors = conditionNodes
          .flatMap(_.cfgNext.l)
          .filter(node => branchEntries.contains(node.id))
          .map(_.id)
          .toSet
        conditionSuccessors.shouldBe(branchEntries)
        conditionNodes
          .flatMap(_.cfgNext.l)
          .filter(node => branchEntries.contains(node.id))
          .map(_.code.trim)
          .toSet
          .shouldBe(Set(thenCall.code.trim, elseCall.code.trim))

        val forkMethod = methodIn(cpg, "probe forkjoin", "forkjoin")
        val fork       = blockWithCode(forkMethod, "fork")
        val branchA    = taskCall(forkMethod, "probe forkjoin>branchA")
        val branchB    = taskCall(forkMethod, "probe forkjoin>branchB")
        successorCodes(fork).shouldBe(Set(branchA.code.trim, branchB.code.trim))
        val loopMethod = methodIn(cpg, "probe loop", "loop")
        val goto       = loopMethod.controlStructure.l.find(_.code.trim == "goto label_0").get
        val jumpTarget = loopMethod.ast.l.collect { case target: JumpTarget if target.name == "label_0" => target }.head
        val loopBody   = taskCall(loopMethod, "probe loop>body")
        val labelBlock = blockWithCode(loopMethod, "label_0:")
        labelBlock.astChildren.l.head.id.shouldBe(jumpTarget.id)
        jumpTarget.order.shouldBe(1)
        jumpTarget.lineNumber.shouldBe(labelBlock.lineNumber)
        jumpTarget.columnNumber.shouldBe(labelBlock.columnNumber)
        goto.cfgNext.l.map(_.id).toSet.shouldBe(Set(jumpTarget.id))
        successorCodes(goto).shouldBe(Set(jumpTarget.code.trim))
        successorCodes(jumpTarget).shouldBe(Set(loopBody.code.trim))
        val loopIf      = loopMethod.controlStructure.l.find(_.code.trim.startsWith("if")).get
        val loopBump    = taskCall(loopMethod, "probe loop>bump")
        val loopCondCfg = (loopIf.condition.l ++ loopIf.condition.ast.l).collect { case node: CfgNode => node }
        val loopTargets = loopCondCfg.flatMap(_.cfgNext.l).map(_.id).toSet
        loopTargets.should(contain(goto.id))
        loopTargets.should(contain(labelBlock.id))
        val loopTargetCodes = loopCondCfg.flatMap(_.cfgNext.l).map(_.code.trim).toSet
        loopTargetCodes.should(contain(goto.code.trim))
        loopTargetCodes.should(contain(labelBlock.code.trim))
        loopBody.cfgNext.l.map(_.id).toSet.should(contain(loopBump.id))

        val subflow = methodIn(cpg, "probe subflow", "subflow")
        val subInit = taskCall(subflow, "probe subflow>init")
        val subCall = taskCall(subflow, "probe subflow>sub")
        subInit.cfgNext.l.map(_.id).toSet.should(contain(subCall.id))

        val select  = methodIn(cpg, "probe select", "select")
        val selInit = taskCall(select, "probe select>init")
        val selCall = taskCall(select, "probe select>dyn")
        selInit.cfgNext.l.map(_.id).toSet.should(contain(selCall.id))

        List("default", "fastpath", "literal", "named", "priority", "sequential").foreach { mode =>
          val prio = methodIn(cpg, s"probe prio $mode", "prio")
          taskCall(prio, s"probe prio $mode>init").cfgNext.l
            .map(_.id)
            .toSet
            .should(contain(taskCall(prio, s"probe prio $mode>ordered").id))
        }

        val modes             = methodIn(cpg, "probe modes", "modes")
        val expectedModeOrder = List(
          "probe modes>init",
          "probe modes>t_RetePlus_Default_None",
          "probe modes>t_RetePlus_Default_Rule",
          "probe modes>t_RetePlus_Default_RuleInstance",
          "probe modes>t_RetePlus_Literal_None",
          "probe modes>t_RetePlus_Literal_Rule",
          "probe modes>t_RetePlus_Literal_RuleInstance",
          "probe modes>t_RetePlus_Priority_None",
          "probe modes>t_RetePlus_Priority_Rule",
          "probe modes>t_RetePlus_Priority_RuleInstance",
          "probe modes>t_Sequential_Default_None",
          "probe modes>t_Sequential_Default_Rule",
          "probe modes>t_Sequential_Default_RuleInstance",
          "probe modes>t_Sequential_Literal_None",
          "probe modes>t_Sequential_Literal_Rule",
          "probe modes>t_Sequential_Literal_RuleInstance",
          "probe modes>t_Sequential_Priority_None",
          "probe modes>t_Sequential_Priority_Rule",
          "probe modes>t_Sequential_Priority_RuleInstance",
          "probe modes>t_Fastpath_Default_None",
          "probe modes>t_Fastpath_Default_Rule",
          "probe modes>t_Fastpath_Default_RuleInstance",
          "probe modes>t_Fastpath_Literal_None",
          "probe modes>t_Fastpath_Literal_Rule",
          "probe modes>t_Fastpath_Literal_RuleInstance",
          "probe modes>t_Fastpath_Priority_None",
          "probe modes>t_Fastpath_Priority_Rule",
          "probe modes>t_Fastpath_Priority_RuleInstance"
        )
        val modeCalls = modes.call.l.filter(_.code.trim.startsWith("call task")).sortBy(_.order)
        modeCalls.map(_.name).shouldBe(expectedModeOrder)
        modeCalls.sliding(2).foreach {
          case Seq(previous, next) => previous.cfgNext.l.map(_.id).toSet.should(contain(next.id))
          case _                   =>
        }
      }
    }

    "join fork branches at the following statement, including an empty branch" in {
      val source = """flowtask f($p) {
        |  {
        |    fork {
        |      call task: f>left;
        |    }
        |    && {
        |      call task: f>right;
        |    }
        |    call task: f>join;
        |  }
        |}
        |""".stripMargin
      val cpg = code(source)
      X2Cpg.applyDefaultOverlays(cpg)
      val method = cpg.method.nameExact("f").head
      val fork   = blockWithCode(method, "fork")
      val left   = method.call.l.find(_.name == "f>left").get
      val right  = method.call.l.find(_.name == "f>right").get
      val join   = method.call.l.find(_.name == "f>join").get
      successorCodes(fork).shouldBe(Set(left.code.trim, right.code.trim))
      left.cfgNext.l.map(_.id).toSet.should(contain(join.id))
      right.cfgNext.l.map(_.id).toSet.should(contain(join.id))
    }

    "use the fork node as the empty branch fringe" in {
      val source = """flowtask f($p) {
        |  {
        |    fork {
        |      call task: f>branch;
        |    }
        |    && {
        |    }
        |    call task: f>join;
        |  }
        |}
        |""".stripMargin
      val cpg = code(source)
      X2Cpg.applyDefaultOverlays(cpg)
      val method = cpg.method.nameExact("f").head
      val fork   = blockWithCode(method, "fork")
      val branch = method.call.l.find(_.name == "f>branch").get
      val join   = method.call.l.find(_.name == "f>join").get
      successorCodes(fork).shouldBe(Set(branch.code.trim, join.code.trim))
      branch.cfgNext.l.map(_.id).toSet.should(contain(join.id))
    }

    "export the parallel fork and label/goto CFG successor ids" in {
      withResourceCpg(Seq("forkjoin", "loop")) { cpg =>
        val json                                       = ujson.read(ArlExport.toJson(cpg))
        val methods                                    = json("methods").arr
        def exportedMethod(name: String, file: String) =
          methods
            .find(method =>
              method("name").str == name &&
                method("filename").str.endsWith(s"/$file.arl") &&
                method("arlKind").str == "flowtask"
            )
            .get

        val forkMethod = methodIn(cpg, "probe forkjoin", "forkjoin")
        val forkNode   = blockWithCode(forkMethod, "fork")
        val forkJson   = exportedMethod("probe forkjoin", "forkjoin")("nodes").arr
          .find(node => node("label").str == "BLOCK" && node("code").str == "fork")
          .get
        forkJson("cfgOut").arr.map(_.num.toLong).toSet.shouldBe(forkNode.cfgNext.l.map(_.id).toSet)

        val loopMethod = methodIn(cpg, "probe loop", "loop")
        val jumpTarget = loopMethod.ast.l.collect { case target: JumpTarget if target.name == "label_0" => target }.head
        val goto       = loopMethod.controlStructure.l.find(_.code.trim == "goto label_0").get
        val loopJson   = exportedMethod("probe loop", "loop")("nodes").arr
        val gotoJson   =
          loopJson.find(node => node("label").str == "CONTROL_STRUCTURE" && node("code").str.trim == "goto label_0").get
        val jumpJson = loopJson.find(node => node("label").str == "JUMP_TARGET" && node("code").str == "label_0:").get
        gotoJson("cfgOut").arr.map(_.num.toLong).toSet.shouldBe(goto.cfgNext.l.map(_.id).toSet)
        jumpJson("cfgOut").arr.map(_.num.toLong).toSet.shouldBe(jumpTarget.cfgNext.l.map(_.id).toSet)
      }
    }

    "annotate ARL methods and report only select predicates as unknown" in {
      withResourceCpg(cfgResources) { cpg =>
        val methods = cpg.method.l.filterNot(_.isExternal).filter(_.filename.endsWith(".arl"))
        val unknown = methods.filter(_.annotation.name("arlKind").l.isEmpty)
        info(
          s"Methods exported with arlKind=unknown: ${unknown.map(method => s"${method.filename}: ${method.fullName}").sorted.mkString(", ")}"
        )
        unknown.forall(_.name.endsWith("$select")).shouldBe(true)

        val branchMethods = methods.filter(_.name == "probe branch")
        branchMethods
          .map(_.annotation.name("arlKind").parameterAssign.value.code.head)
          .toSet
          .shouldBe(Set("flowtask", "ruleflow"))
        val branchRuleKind = methods
          .find(_.name == "probe branch>hi")
          .get
          .annotation
          .name("arlKind")
          .parameterAssign
          .value
          .code
          .head
        branchRuleKind.shouldBe("ruletask")
        val branchFunctionKind = methods
          .find(_.name == "probe branch>init")
          .get
          .annotation
          .name("arlKind")
          .parameterAssign
          .value
          .code
          .head
        branchFunctionKind.shouldBe("functiontask")
        methods
          .map(_.annotation.name("arlKind").parameterAssign.value.code.headOption.getOrElse("unknown"))
          .toSet
          .should(contain("rule"))

        val root = ujson.read(ArlExport.toJson(cpg))
        root.obj.keys.toList.shouldBe(List("methods"))
        root("methods").arr.foreach(method => method.obj.keys.toList.should(contain("arlKind")))
        val exportedUnknown = root("methods").arr.filter(_("arlKind").str == "unknown").map(_("fullName").str).sorted
        exportedUnknown.shouldBe(unknown.map(_.fullName).sorted)
        val methodKeyOrder = root("methods").arr.head.obj.keys.toList
        methodKeyOrder.shouldBe(List("id", "name", "fullName", "signature", "filename", "line", "arlKind", "nodes"))
        val methodSortKeys = root("methods").arr.map(method => (method("fullName").str, method("id").num.toLong)).toList
        methodSortKeys.shouldBe(methodSortKeys.sorted)
      }
    }
  }

  "ARL export" should {
    "export logical-and condition ids and ordered argument codes" in {
      val source = """flowtask f($p) {
        |  {
        |    if (a.hasNext() && b) {
        |    }
        |  }
        |}
        |""".stripMargin
      val cpg = code(source)
      X2Cpg.applyDefaultOverlays(cpg)
      val root   = ujson.read(ArlExport.toJson(cpg))
      val method = root("methods").arr.find(_("name").str == "f").get
      val nodes  = method("nodes").arr
      val byId   = nodes.map(node => node("id").num.toLong -> node).toMap
      val ifNode = nodes
        .find(node =>
          node("label").str == "CONTROL_STRUCTURE" && node.obj.get("controlStructureType").exists(_.str == "IF")
        )
        .get
      val conditionId = ifNode("condition").num.toLong
      val condition   = byId(conditionId)
      condition("label").str.shouldBe("CALL")
      condition("name").str.shouldBe(Operators.logicalAnd)
      condition("arguments").arr.map(id => byId(id.num.toLong)("code").str).toList.shouldBe(List("a.hasNext()", "b"))
      condition.obj.keys.toList.shouldBe(
        List(
          "id",
          "label",
          "order",
          "code",
          "line",
          "columnNumber",
          "children",
          "cfgOut",
          "name",
          "methodFullName",
          "signature",
          "typeFullName",
          "arguments",
          "callees"
        )
      )
      ifNode.obj.keys.toList.shouldBe(
        List(
          "id",
          "label",
          "order",
          "code",
          "line",
          "columnNumber",
          "children",
          "cfgOut",
          "controlStructureType",
          "condition"
        )
      )
      nodes.map(_("id").num.toLong).toList.shouldBe(nodes.map(_("id").num.toLong).sorted)
    }

    "produce valid byte-identical JSON for independent builds of identical input" in {
      val source = """flowtask deterministic($p) {
        |  {
        |    if (a.hasNext() && b) {
        |    }
        |  }
        |}
        |""".stripMargin
      FileUtil.usingTemporaryDirectory("arl2cpg-export-determinism") { dir =>
        Files.writeString(dir.resolve("deterministic.arl"), source)

        def exportJson(): String = {
          val cpg = new Arl2Cpg().createCpg(Config().withInputPath(dir.toString)).get
          try {
            X2Cpg.applyDefaultOverlays(cpg)
            ArlExport.toJson(cpg)
          } finally {
            cpg.close()
          }
        }

        val first  = exportJson()
        val second = exportJson()
        ujson.read(first)
        ujson.read(second)
        first.shouldBe(second)
      }
    }
  }
}
