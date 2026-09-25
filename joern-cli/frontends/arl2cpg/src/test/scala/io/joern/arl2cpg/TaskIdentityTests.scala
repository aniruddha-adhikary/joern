package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class TaskIdentityTests extends Arl2CpgSuite {

  private val uuid1 = "11111111-1111-1111-1111-111111111111"
  private val uuid2 = "22222222-2222-2222-2222-222222222222"

  /** ARL with two `flowtask DCS_Flow>Common` declarations at lines 4 and 5. */
  private val twinArl = """ruleset Twin (S){
  rule `r.1` { then { } }
}
flowtask `DCS_Flow>Common` ($p) { { } }
flowtask `DCS_Flow>Common` ($p) { { } }
"""

  /** ARL where two `flowtask Main` call `Step`, itself declared twice (lines 4,5,6,7). */
  private val callArl = """ruleset Twin (S){
  rule `r.1` { then { } }
}
flowtask Main ($p) { { call task: Step; } }
flowtask Main ($p) { { call task: Step; } }
flowtask Step ($p) { { } }
flowtask Step ($p) { { } }
"""

  private def record(line: Int, task: String, uuid: String, extra: List[(String, String)] = List.empty): String = {
    val fields =
      List("file" -> "\"test.arl\"", "line" -> line.toString, "task" -> s"\"$task\"", "uuid" -> s"\"$uuid\"") ++
        extra.map { case (k, v) => k -> s"\"$v\"" }
    fields.map { case (k, v) => s"\"$k\":$v" }.mkString("{", ",", "}")
  }

  private def writeSidecar(lines: List[String]): Path = {
    val file = Files.createTempFile("arl2cpg-identity", ".jsonl")
    Files.writeString(file, lines.mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    file
  }

  private def arlCpg(arl: String, sidecar: Option[Path] = None, rflDir: Option[Path] = None): Cpg = {
    val srcDir = Files.createTempDirectory("arl2cpg-src-test")
    Files.writeString(srcDir.resolve("test.arl"), arl, StandardCharsets.UTF_8)
    var config = Config().withInputPath(srcDir.toString)
    sidecar.foreach(path => config = config.withTaskIdentityPaths(Set(path.toString)))
    rflDir.foreach(dir => config = config.withRflSrcPaths(Set(dir.toString)))
    new Arl2Cpg().createCpg(config).get
  }

  private def writeRfls(entries: (String, String, String, List[String])*): Path = {
    val dir = Files.createTempDirectory("arl2cpg-rfl-test")
    entries.foreach { case (fileName, flowName, uuid, taskIds) =>
      val tasks = taskIds.map(id => s"""<RuleTask Identifier="$id"/>""").mkString("\n")
      Files.writeString(
        dir.resolve(fileName),
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
      )
    }
    dir
  }

  private def annValue(method: Method, annoName: String): Option[String] =
    method.annotation.name(annoName).parameterAssign.value.headOption.map(_.code)

  private def annValue(method: Method, annoName: String, default: String): String =
    annValue(method, annoName).getOrElse(default)

  "two same-named tasks with identity records" should {

    val sidecar = writeSidecar(
      List(
        record(4, "DCS_Flow>Common", uuid1, List("qualifiedName" -> "com.acme.dcs.Common")),
        record(5, "DCS_Flow>Common", uuid2)
      )
    )
    val cpg = arlCpg(twinArl, Option(sidecar))

    "be scoped to the record uuids" in {
      val commons = cpg.method.name("DCS_Flow>Common").l
      commons.size.shouldBe(2)
      commons
        .map(_.fullName)
        .toSet
        .shouldBe(Set(s"Twin.DCS_Flow>Common@$uuid1:void()", s"Twin.DCS_Flow>Common@$uuid2:void()"))
      commons.map(m => annValue(m, "ruleflowScope", "")).toSet.shouldBe(Set("resolved"))
      commons.map(m => annValue(m, "taskIdentitySource", "")).toSet.shouldBe(Set("sidecar"))
      commons.map(m => annValue(m, "taskIdentityVerified", "")).toSet.shouldBe(Set("false"))
    }

    "carry the qualifiedName only on the record that has one" in {
      val qualified = cpg.method.name("DCS_Flow>Common").l.flatMap(m => annValue(m, "taskQualifiedName"))
      qualified.shouldBe(List("com.acme.dcs.Common"))
    }
  }

  "identity records carrying arlSha256" should {

    val sha = io.joern.arl2cpg.identity.TaskIdentity.sha256Hex(twinArl.getBytes(StandardCharsets.UTF_8))

    "mark tasks verified when the hash matches" in {
      val sidecar = writeSidecar(
        List(
          record(4, "DCS_Flow>Common", uuid1, List("arlSha256" -> sha)),
          record(5, "DCS_Flow>Common", uuid2, List("arlSha256" -> sha))
        )
      )
      val cpg     = arlCpg(twinArl, Option(sidecar))
      val commons = cpg.method.name("DCS_Flow>Common").l
      commons.map(m => annValue(m, "taskIdentityVerified", "")).toSet.shouldBe(Set("true"))
    }

    "ignore the sidecar when the hash mismatches" in {
      val sidecar = writeSidecar(
        List(
          record(4, "DCS_Flow>Common", uuid1, List("arlSha256" -> "deadbeef")),
          record(5, "DCS_Flow>Common", uuid2, List("arlSha256" -> "deadbeef"))
        )
      )
      val cpg     = arlCpg(twinArl, Option(sidecar))
      val commons = cpg.method.name("DCS_Flow>Common").l
      commons.size.shouldBe(2)
      commons.map(_.fullName).toSet.shouldBe(Set("Twin.DCS_Flow>Common:void()"))
      commons.map(m => annValue(m, "ruleflowScope", "")).toSet.shouldBe(Set("unknown"))
    }
  }

  "a record on the wrong declaration line" should {

    val sidecar = writeSidecar(List(record(99, "DCS_Flow>Common", uuid1)))
    val cpg     = arlCpg(twinArl, Option(sidecar))

    "leave the task unscoped" in {
      val commons = cpg.method.name("DCS_Flow>Common").l
      commons.size.shouldBe(2)
      commons.map(_.fullName).toSet.shouldBe(Set("Twin.DCS_Flow>Common:void()"))
      commons.map(m => annValue(m, "ruleflowScope", "")).toSet.shouldBe(Set("ambiguous"))
    }
  }

  "call task resolution through identity scope" should {

    val sidecar = writeSidecar(
      List(record(4, "Main", uuid1), record(5, "Main", uuid2), record(6, "Step", uuid1), record(7, "Step", uuid2))
    )
    val cpg = arlCpg(callArl, Option(sidecar))

    "resolve each duplicated target through its caller's uuid" in {
      val main1 = cpg.method.fullNameExact(s"Twin.Main@$uuid1:void()").head
      main1.ast.isCall.name("Step").methodFullName.l.shouldBe(List(s"Twin.Step@$uuid1:void()"))
      val main2 = cpg.method.fullNameExact(s"Twin.Main@$uuid2:void()").head
      main2.ast.isCall.name("Step").methodFullName.l.shouldBe(List(s"Twin.Step@$uuid2:void()"))
    }
  }

  "identity records alongside .rfl metadata" should {

    val rflDir =
      writeRfls(("a.rfl", "FlowA", uuid1, List("DCS_Flow>Common")), ("b.rfl", "FlowB", uuid2, List("DCS_Flow>Common")))
    val sidecar = writeSidecar(List(record(4, "DCS_Flow>Common", uuid2)))
    val cpg     = arlCpg(twinArl, Option(sidecar), Option(rflDir))

    "let the sidecar win over rfl inference" in {
      val scoped = cpg.method.fullNameExact(s"Twin.DCS_Flow>Common@$uuid2:void()").l
      scoped.size.shouldBe(1)
      annValue(scoped.head, "ruleflowName", "").shouldBe("FlowB")
      annValue(scoped.head, "ruleflowUuid", "").shouldBe(uuid2)
    }
  }

  "conflicting records for the same key" should {

    val sidecar = writeSidecar(List(record(4, "DCS_Flow>Common", uuid1), record(4, "DCS_Flow>Common", uuid2)))
    val cpg     = arlCpg(twinArl, Option(sidecar))

    "drop both rather than pick one" in {
      val commons = cpg.method.name("DCS_Flow>Common").l
      commons.size.shouldBe(2)
      commons.map(_.fullName).toSet.shouldBe(Set("Twin.DCS_Flow>Common:void()"))
      commons.map(m => annValue(m, "ruleflowScope", "")).toSet.shouldBe(Set("unknown"))
    }
  }

  "a sidecar with a malformed line" should {

    val sidecar = writeSidecar(List("{not json", record(5, "DCS_Flow>Common", uuid2)))
    val cpg     = arlCpg(twinArl, Option(sidecar))

    "skip it and still apply the valid records" in {
      val commons = cpg.method.name("DCS_Flow>Common").l
      commons.size.shouldBe(2)
      commons.map(_.fullName).toSet.shouldBe(Set("Twin.DCS_Flow>Common:void()", s"Twin.DCS_Flow>Common@$uuid2:void()"))
    }
  }
}
