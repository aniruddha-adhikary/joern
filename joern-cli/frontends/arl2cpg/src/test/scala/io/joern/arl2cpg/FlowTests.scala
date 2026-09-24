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
ruletask select_rules (ctx) {
  ordering: natural;
  rules: chain.*;
}
"""

  "ruleflow" should {
    "lower to a METHOD whose body calls the maintask" in {
      val cpg  = code(arl)
      val flow = cpg.method.name("main").headOption
      flow should not be empty
      flow.get.fullName shouldBe "R.main:void()"
      val calls = flow.get.call.name("body").l
      calls.size shouldBe 1
      calls.head.methodFullName shouldBe "R.body:void()"
    }
  }

  "ruletask rules selector" should {
    "emit one call per matching rule" in {
      val cpg      = code(arl)
      val ruletask = cpg.method.name("select_rules").headOption
      ruletask should not be empty
      val calls =
        ruletask.get.call.l.filter(c => c.methodFullName.endsWith(":void()") && c.methodFullName.contains(".chain."))
      calls.map(_.methodFullName).toSet shouldBe Set("R.chain.a:void()", "R.chain.b:void()")
      ruletask.get.annotation.name.l should contain("rules")
      ruletask.get.annotation.name.l should contain("ordering")
    }
  }

  "flow statements" should {
    "lower call task to the last part" in {
      val cpg   = code(arl)
      val calls = cpg.method.name("body").call.name("select_rules").l
      calls.size shouldBe 1
      calls.head.methodFullName shouldBe "R.select_rules:void()"
    }

    "lower fork/goto/labels" in {
      val cpg = code(arl)
      cpg.block.code("fork").size shouldBe 1
      cpg.controlStructure.code("goto done").size shouldBe 1
      cpg.block.code("done:").size shouldBe 1
    }
  }
}
