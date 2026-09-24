package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class FlowTests extends Arl2CpgSuite {

  private val arl = """import java.lang.Object;
ruleset R (S){
  rule `chain.a` { then { } }
  rule `chain.b` { then { } }
  rule `other.c` { then { } }
}
ruleflow main ($p) { maintask body; }
flowtask body ($p) {
  {
    call task: body > select_rules;
    if ($p > 0) {
      goto done;
    }
    done: {
      fork {
        call task: body > a;
      } && {
        call task: body > b;
      }
    }
  }
}
ruletask `body>select_rules` (ctx) {
  ordering: natural;
  rules: chain.*;
}
"""

  "ruleflow" should {
    "lower to a METHOD whose body calls the maintask" in {
      val cpg  = code(arl)
      val flow = cpg.method.name("main").headOption
      flow should not be empty
      flow.get.fullName shouldBe "R.ruleflow.main:void()"
      val calls = flow.get.call.name("body").l
      calls.size shouldBe 1
      calls.head.methodFullName shouldBe "R.body:void()"
    }
  }

  "rules selectors containing spaces" should {

    val spacedArl = """ruleset R (S){
        |  rule `GBP D_01` { then { } }
        |  rule `GBP D_02` { then { } }
        |  rule `other.x` { then { } }
        |}
        |ruletask exact_sel (c) { rules : GBP D_01; }
        |ruletask pair_sel (c) { rules : GBP D_01, other.x; }
        |ruletask prefix_sel (c) { rules : other.*; }
        |""".stripMargin

    "preserve whitespace inside rule names (exact selector)" in {
      val cpg   = code(spacedArl)
      val calls = cpg.method.name("exact_sel").call.name("GBP D_01").l
      calls.size shouldBe 1
      calls.head.methodFullName shouldBe "R.GBP D_01:void()"
    }

    "match a comma-separated selector list" in {
      val cpg = code(spacedArl)
      cpg.method.name("pair_sel").call.methodFullName.l.toSet shouldBe
        Set("R.GBP D_01:void()", "R.other.x:void()")
    }

    "still match a prefix selector" in {
      val cpg = code(spacedArl)
      cpg.method.name("prefix_sel").call.methodFullName.l shouldBe List("R.other.x:void()")
    }

    "emit a call to the spaced rule from both ruletasks" in {
      val cpg = code(spacedArl)
      cpg.call.methodFullNameExact("R.GBP D_01:void()").size shouldBe 2
    }

    "preserve the space in the rules annotation code" in {
      val cpg   = code(spacedArl)
      val codes = cpg.method.name("exact_sel").annotation.name("rules").code.l
      codes.size shouldBe 1
      codes.head should include("GBP D_01")
    }
  }

  "ruletask rules selector" should {
    "emit one call per matching rule" in {
      val cpg      = code(arl)
      val ruletask = cpg.method.name("body>select_rules").headOption
      ruletask should not be empty
      val calls =
        ruletask.get.call.l.filter(c => c.methodFullName.endsWith(":void()") && c.methodFullName.contains(".chain."))
      calls.map(_.methodFullName).toSet shouldBe Set("R.chain.a:void()", "R.chain.b:void()")
      ruletask.get.annotation.name.l should contain("rules")
      ruletask.get.annotation.name.l should contain("ordering")
    }
  }

  "ruletask select block" should {

    val selArl = """ruleset R (S){
  rule `a.1` { then { } }
  rule `a.2` { then { } }
  rule `b.1` { then { } }
}
ruletask dyn (ctx) {
  ordering: natural;
  rules : ;
  select (Object r) { return true; }
}
ruletask filtered (ctx) {
  ordering: natural;
  rules : a.*;
  select (Object r) { return true; }
}
ruletask plain (ctx) {
  ordering: natural;
  rules : b.*;
}
"""

    "wrap all file rules in a dynamicSelect when the rules clause is empty" in {
      val cpg = code(selArl)
      val dyn = cpg.method.name("dyn").call.name("<operator>.dynamicSelect").l
      dyn.size shouldBe 1
      val args = dyn.head.argument.l
      args.collect { case m: MethodRef => m }.size shouldBe 1
      val ruleCalls = args.collect { case c: Call => c }
      ruleCalls.map(_.name).sorted shouldBe List("a.1", "a.2", "b.1")
      ruleCalls.map(_.methodFullName).sorted shouldBe List("R.a.1:void()", "R.a.2:void()", "R.b.1:void()")
      cpg.method.name("dyn").annotation.name("rules").code.l shouldBe List("rules: ")
      cpg.method.name("dyn").annotation.name("selection").code.l shouldBe List("selection: dynamic")
    }

    "carry <dynamic> as the rules annotation value" in {
      val cpg   = code(selArl)
      val rules = cpg.method.name("dyn").annotation.name("rules").head
      rules.parameterAssign.value.head.code shouldBe "<dynamic>"
    }

    "restrict the candidates to the matched selector" in {
      val cpg = code(selArl)
      val dyn = cpg.method.name("filtered").call.name("<operator>.dynamicSelect").l
      dyn.size shouldBe 1
      val ruleCalls = dyn.head.argument.collect { case c: Call => c }
      ruleCalls.map(_.name).sorted shouldBe List("a.1", "a.2")
      cpg.method.name("filtered").annotation.name("selection").head.parameterAssign.value.head.code shouldBe "dynamic"
    }

    "keep bare rule calls and a static selection without a select block" in {
      val cpg = code(selArl)
      cpg.method.name("plain").call.name("<operator>.dynamicSelect").size shouldBe 0
      cpg.method.name("plain").call.name("b.1").methodFullName.l shouldBe List("R.b.1:void()")
      cpg.method.name("plain").annotation.name("selection").head.parameterAssign.value.head.code shouldBe "static"
    }
  }

  "flow statements" should {
    "lower call task to the full flow>task name" in {
      val cpg   = code(arl)
      val calls = cpg.method.name("body").call.name("body>select_rules").l
      calls.size shouldBe 1
      calls.head.methodFullName shouldBe "R.body>select_rules:void()"
      cpg.method.name("body>select_rules").size shouldBe 1
    }

    "link a multi-word backticked flow>task call" in {
      val cpg = code("""ruleset R (S){ rule `r` { then { } } }
          |ruleflow `probe x`($p){ maintask `probe x`; }
          |flowtask `probe x`($p){ { call task : probe x>init; } }
          |functiontask `probe x>init`{ { } }
          |""".stripMargin)
      cpg.call.name("probe x>init").methodFullName.l shouldBe List("R.probe x>init:void()")
      cpg.method.name("probe x").fullName.l should contain("R.ruleflow.probe x:void()")
      cpg.method.name("probe x").call.name("probe x>init").methodFullName.l shouldBe
        List("R.probe x>init:void()")
    }

    "lower fork/goto/labels" in {
      val cpg = code(arl)
      cpg.block.code("fork").size shouldBe 1
      cpg.controlStructure.code("goto done").size shouldBe 1
      cpg.block.code("done:").size shouldBe 1
    }
  }
}
