package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.shiftleft.semanticcpg.language.*

class ContainerTests extends Arl2CpgSuite {

  private def fileFor(pkg: String) = s"""package $pkg;
ruleset R (S){
  rule `x.r` { then { } }
}
flowtask init ($$p) {
  {
    call task: init > step;
  }
}
functiontask `init>step` {
  { }
}
ruletask `init>rules` (ctx) {
  ordering: natural;
  rules: x.*;
}
"""

  "two files declaring the same ruleset/task names in different packages" should {

    val cpg = code(fileFor("a"), "a.arl").moreCode(fileFor("b"), "b.arl")

    "produce package-qualified container TYPE_DECLs" in {
      cpg.typeDecl.name("R").fullName.l.sorted.shouldBe(List("a.R", "b.R"))
    }

    "produce distinct METHOD fullNames per package" in {
      cpg.method.name("init").fullName.l.sorted.shouldBe(List("a.R.init:void()", "b.R.init:void()"))
    }

    "keep call task targets file-local" in {
      val aCallers = cpg.call.methodFullNameExact("a.R.init>step:void()").method.fullName.l
      val bCallers = cpg.call.methodFullNameExact("b.R.init>step:void()").method.fullName.l
      aCallers.shouldBe(List("a.R.init:void()"))
      bCallers.shouldBe(List("b.R.init:void()"))
    }

    "keep rules: selector calls file-local" in {
      val aCallers = cpg.call.methodFullNameExact("a.R.x.r:void()").method.fullName.l
      val bCallers = cpg.call.methodFullNameExact("b.R.x.r:void()").method.fullName.l
      aCallers.shouldBe(List("a.R.init>rules:void()"))
      bCallers.shouldBe(List("b.R.init>rules:void()"))
    }
  }

  "a packaged ruleset" should {

    val cpg = code("""package p;
ruleset R (S){
  rule `r.1` { then { } }
}
ruleflow main ($p) { maintask body; }
flowtask body ($p) { { } }
""")

    "prefix the ruleset container fullName with the package" in {
      cpg.typeDecl.name("R").fullName.l.shouldBe(List("p.R"))
      cpg.method.name("r.1").fullName.l.shouldBe(List("p.R.r.1:void()"))
    }

    "prefix flowtask and ruleflow method fullNames" in {
      cpg.method.name("body").fullName.l.shouldBe(List("p.R.body:void()"))
      cpg.method.name("main").fullName.l.shouldBe(List("p.R.ruleflow.main:void()"))
    }
  }
}
