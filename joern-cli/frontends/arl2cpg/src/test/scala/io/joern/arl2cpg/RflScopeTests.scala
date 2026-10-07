package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*

import java.nio.file.{Files, Path}

class RflScopeTests extends Arl2CpgSuite {

  private val uuid1 = "22222222-2222-2222-2222-222222222222"
  private val uuid2 = "33333333-3333-3333-3333-333333333333"

  private def rflXml(flowName: String, uuid: String, taskIds: List[String]): String = {
    val tasks = taskIds.map(id => s"""<RuleTask Identifier="$id"/>""").mkString("\n")
    s"""<?xml version="1.0" encoding="UTF-8"?>
<RflRuleflow>
  <name>$flowName</name>
  <uuid>$uuid</uuid>
  <rfModel><Ruleflow><Body>
    <TaskList>
$tasks
    </TaskList>
  </Body></Ruleflow></rfModel>
</RflRuleflow>
"""
  }

  private def writeRfls(entries: (String, String, String, List[String])*): Path = {
    val dir = Files.createTempDirectory("arl2cpg-rfl-test")
    entries.foreach { case (fileName, flowName, uuid, taskIds) =>
      Files.writeString(dir.resolve(fileName), rflXml(flowName, uuid, taskIds))
    }
    dir
  }

  /** Builds a frontend CPG directly (no default overlays / validation), so tests can inspect same-fullName methods and
    * unresolved calls without call-linker stubs or the post-frontend validator interfering.
    */
  private def arlCpg(arl: String, rflDir: Option[Path]): Cpg = {
    val srcDir = Files.createTempDirectory("arl2cpg-src-test")
    Files.writeString(srcDir.resolve("test.arl"), arl)
    var config = Config().withInputPath(srcDir.toString)
    rflDir.foreach(dir => config = config.withRflSrcPaths(Set(dir.toString)))
    new Arl2Cpg().createCpg(config).get
  }

  private def twinArl(mainA: String, mainB: String): String = s"""ruleset Twin (S){
  rule `r.1` { then { } }
}
ruleflow TwinFlow ($$p) { maintask Main; }
flowtask Main ($$p) { { $mainA } }
flowtask Main ($$p) { { $mainB } }
flowtask Step ($$p) { { call task: Plain; } }
flowtask Step ($$p) { { call task: Amended; } }
ruletask Plain (ctx) { ordering: natural; rules : r.*; }
ruletask Amended (ctx) { ordering: natural; rules : r.*; }
"""

  private def flowNameArl(bodies: List[String], duplicateComputation: Boolean = false): String = {
    val flows = bodies.map { body =>
      s"""flowtask Flow ($$p) {
         |  { $body }
         |}""".stripMargin
    }
    val computationCount = if (duplicateComputation) 2 else 1
    val computations     = List
      .fill(computationCount)("ruletask computation (ctx) { ordering: natural; rules: r.*; }")
      .mkString("\n")
    s"""ruleset Twin (S){
       |  rule `r.1` { then { } }
       |}
       |${flows.mkString("\n")}
       |$computations
       |ruletask check (ctx) { ordering: natural; rules: r.*; }
       |ruletask GET_NEXT_ITEM (ctx) { ordering: natural; rules: r.*; }
       |ruletask other (ctx) { ordering: natural; rules: r.*; }
       |""".stripMargin
  }

  private def scopeOf(method: Method): String =
    method.annotation.name("ruleflowScope").parameterAssign.value.head.code

  private def uuidOf(method: Method): String =
    method.annotation.name("ruleflowUuid").parameterAssign.value.head.code

  "two flowtasks sharing a name" should {

    val dir = writeRfls(
      ("plain.rfl", "Twin", uuid1, List("Main", "Step", "Plain")),
      ("amend.rfl", "Twin", uuid2, List("Main", "Step", "Amended"))
    )
    val cpg = arlCpg(twinArl("call task: Plain; call task: Step;", "call task: Amended; call task: Step;"), Option(dir))

    "be scoped to distinct uuids" in {
      val mains = cpg.method.name("Main").l
      mains.size.shouldBe(2)
      mains.map(_.fullName).toSet.shouldBe(Set(s"Twin.Main@$uuid1:void()", s"Twin.Main@$uuid2:void()"))
      mains.map(scopeOf).toSet.shouldBe(Set("resolved"))
      mains.foreach { method =>
        if (method.fullName.contains(uuid1)) uuidOf(method).shouldBe(uuid1)
        else uuidOf(method).shouldBe(uuid2)
      }
    }

    "keep non-duplicated call targets unscoped" in {
      cpg.method
        .fullNameExact(s"Twin.Main@$uuid1:void()")
        .ast
        .isCall
        .name("Plain")
        .methodFullName
        .l
        .shouldBe(List("Twin.Plain:void()"))
    }

    "resolve a duplicated call target through the caller's scope" in {
      cpg.method
        .fullNameExact(s"Twin.Main@$uuid1:void()")
        .ast
        .isCall
        .name("Step")
        .methodFullName
        .l
        .shouldBe(List(s"Twin.Step@$uuid1:void()"))
      cpg.method
        .fullNameExact(s"Twin.Main@$uuid2:void()")
        .ast
        .isCall
        .name("Step")
        .methodFullName
        .l
        .shouldBe(List(s"Twin.Step@$uuid2:void()"))
    }
  }

  "a flowtask named after its ruleflow" should {
    val metadata = writeRfls(
      ("short.rfl", "Flow", uuid1, List("computation", "check")),
      ("long.rfl", "Flow", uuid2, List("computation", "check", "GET_NEXT_ITEM"))
    )
    val shortBody = "call task: Flow>computation; call task: Flow>check;"
    val longBody  = s"$shortBody call task: Flow>GET_NEXT_ITEM;"

    "scope copies by contained qualified call targets regardless of source order" in {
      def assertScopes(bodies: List[String], expectedUuids: List[String], expectedExtraCalls: List[Boolean]): Unit = {
        val cpg = arlCpg(flowNameArl(bodies), Option(metadata))
        try {
          val flows = cpg.method.nameExact("Flow").l.sortBy(_.lineNumber)
          flows.size.shouldBe(2)
          flows.map(_.fullName).shouldBe(expectedUuids.map(uuid => s"Twin.Flow@$uuid:void()"))
          flows.map(scopeOf).shouldBe(List("resolved", "resolved"))
          flows.map(uuidOf).shouldBe(expectedUuids)
          flows.map(_.ast.isCall.nameExact("Flow>GET_NEXT_ITEM").nonEmpty).shouldBe(expectedExtraCalls)
        } finally cpg.close()
      }

      assertScopes(List(shortBody, longBody), List(uuid1, uuid2), List(false, true))
      assertScopes(List(longBody, shortBody), List(uuid2, uuid1), List(true, false))
    }

    "leave non-contained name-matched calls unscoped without blocking a unique match" in {
      val onlyComputation = writeRfls(("flow.rfl", "Flow", uuid1, List("computation")))
      val cpg             =
        arlCpg(flowNameArl(List("call task: Flow>computation;", "call task: Flow>other;")), Option(onlyComputation))
      try {
        val flows = cpg.method.nameExact("Flow").l.sortBy(_.lineNumber)
        flows.size.shouldBe(2)
        flows.map(_.fullName).shouldBe(List(s"Twin.Flow@$uuid1:void()", "Twin.Flow:void()"))
        flows.map(scopeOf).shouldBe(List("resolved", "ambiguous"))
        flows.head.annotation.name("ruleflowUuid").parameterAssign.value.head.code.shouldBe(uuid1)
        flows(1).annotation.name("ruleflowUuid").l.shouldBe(Nil)
      } finally cpg.close()
    }

    "leave indistinguishable single-meta copies unscoped" in {
      val onlyComputation = writeRfls(("flow.rfl", "Flow", uuid1, List("computation")))
      val cpg             = arlCpg(
        flowNameArl(List("call task: Flow>computation;", "call task: Flow>computation;")),
        Option(onlyComputation)
      )
      try {
        val flows = cpg.method.nameExact("Flow").l
        flows.size.shouldBe(2)
        flows.map(_.fullName).toSet.shouldBe(Set("Twin.Flow:void()"))
        flows.map(scopeOf).toSet.shouldBe(Set("ambiguous"))
        flows.foreach(_.annotation.name("ruleflowUuid").l.shouldBe(Nil))
      } finally cpg.close()
    }

    "report call targets when the computation declaration is duplicated" in {
      val cpg = arlCpg(flowNameArl(List(shortBody, longBody), duplicateComputation = true), Option(metadata))
      try {
        val flows        = cpg.method.nameExact("Flow").l.sortBy(_.lineNumber)
        val observations = flows.map { flow =>
          val target = flow.ast.isCall.nameExact("Flow>computation").methodFullName.head
          s"$target (matching METHODs: ${cpg.method.fullNameExact(target).size})"
        }
        info(s"Duplicate computation target observation: ${observations.mkString("; ")}")
      } finally cpg.close()
    }
  }

  "indistinguishable twin bodies" should {

    val dir = writeRfls(
      ("plain.rfl", "Twin", uuid1, List("Main", "Plain")),
      ("amend.rfl", "Twin", uuid2, List("Main", "Plain"))
    )
    val cpg = arlCpg(twinArl("call task: Plain;", "call task: Plain;"), Option(dir))

    "keep the shared fullName and report ambiguous" in {
      val mains = cpg.method.name("Main").l
      mains.size.shouldBe(2)
      mains.map(_.fullName).toSet.shouldBe(Set("Twin.Main:void()"))
      mains.map(scopeOf).toSet.shouldBe(Set("ambiguous"))
    }
  }

  "select predicates of scoped duplicate ruletasks" should {

    val dir = writeRfls(("a.rfl", "A", uuid1, List("Step")), ("b.rfl", "B", uuid2, List("Step")))
    val cpg = arlCpg(
      """ruleset Twin (S){
  rule `r.1` { then { } }
}
ruletask `A>Step` (ctx) { ordering: natural; rules: r.*; select (Object r) { return true; } }
ruletask `A>Step` (ctx) { ordering: natural; rules: r.*; select (Object r) { return true; } }
ruletask `B>Step` (ctx) { ordering: natural; rules: r.*; select (Object r) { return true; } }
ruletask `B>Step` (ctx) { ordering: natural; rules: r.*; select (Object r) { return true; } }
""",
      Option(dir)
    )

    "emit select METHODs whose METHOD_REFs resolve to them" in {
      val methodFullNames = cpg.method.filter(_.name.endsWith("$select")).fullName.l.toSet
      methodFullNames.shouldBe(
        Set(
          s"Twin.A>Step$$select@$uuid1:boolean(java.lang.Object)",
          s"Twin.A>Step$$select@$uuid1:boolean(java.lang.Object)",
          s"Twin.B>Step$$select@$uuid2:boolean(java.lang.Object)",
          s"Twin.B>Step$$select@$uuid2:boolean(java.lang.Object)"
        )
      )
      val scopedRefTargets =
        cpg.methodRef.filter(_.methodFullName.contains("$select")).methodFullName.l.toSet
      scopedRefTargets.shouldBe(
        Set(
          s"Twin.A>Step$$select@$uuid1:boolean(java.lang.Object)",
          s"Twin.B>Step$$select@$uuid2:boolean(java.lang.Object)"
        )
      )
      scopedRefTargets.subsetOf(methodFullNames).shouldBe(true)
    }
  }

  "a task qualified to a flow not in the metadata" should {

    val dir = writeRfls(("b.rfl", "B", uuid2, List("Step")))
    val cpg = arlCpg(
      """ruleset Twin (S){
  rule `r.1` { then { } }
}
ruletask `A>Step` (ctx) { ordering: natural; rules: r.*; }
ruletask `A>Step` (ctx) { ordering: natural; rules: r.*; }
""",
      Option(dir)
    )

    "stay unscoped instead of matching by bare id" in {
      val steps = cpg.method.name("A>Step").l
      steps.size.shouldBe(2)
      steps.map(_.fullName).toSet.shouldBe(Set("Twin.A>Step:void()"))
      steps.map(scopeOf).toSet.shouldBe(Set("ambiguous"))
      steps.flatMap(_.annotation.name("ruleflowUuid").l).shouldBe(Nil)
    }
  }

  "a scoped caller calling a qualified cross-flow target" should {

    val dir = writeRfls(("a.rfl", "A", uuid1, List("Main", "Sub")), ("b.rfl", "B", uuid2, List("Main")))
    val cpg = arlCpg(
      """ruleset Twin (S){
  rule `r.1` { then { } }
}
flowtask Main ($p) { { call task: Sub; } }
flowtask Main ($p) { { } }
flowtask Sub ($p) { { call task: B>Main; } }
""",
      Option(dir)
    )

    "resolve the caller's scope and the qualified target's uuid" in {
      val sub = cpg.method.name("Sub").head
      scopeOf(sub).shouldBe("resolved")
      uuidOf(sub).shouldBe(uuid1)
      sub.fullName.shouldBe("Twin.Sub:void()")
      sub.ast.isCall.name("B>Main").methodFullName.l.shouldBe(List(s"Twin.B>Main@$uuid2:void()"))
    }
  }

  "tasks without any rfl metadata" should {

    val cpg = arlCpg(twinArl("call task: Plain;", "call task: Amended;"), None)

    "keep plain fullNames and report unknown" in {
      val mains = cpg.method.name("Main").l
      mains.size.shouldBe(2)
      mains.map(_.fullName).toSet.shouldBe(Set("Twin.Main:void()"))
      mains.map(scopeOf).toSet.shouldBe(Set("unknown"))
    }
  }
}
