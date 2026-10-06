package io.joern.arl2cpg

import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.{Codes, Keys}
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlExport
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class EngineDataTypingTests extends AnyWordSpec with Matchers {

  private val xomSourceDir = Paths.get(getClass.getResource("/xom").toURI).toString

  private def withCpg(sources: Map[String, String], b2x: Option[String] = None)(test: Cpg => Unit): Unit =
    FileUtil.usingTemporaryDirectory("arl2cpg-engine-data-test") { dir =>
      sources.toList.sortBy(_._1).foreach { case (relativePath, code) =>
        val file = dir.resolve(relativePath)
        Files.createDirectories(file.getParent)
        Files.writeString(file, code)
      }
      var config = Config()
        .withInputPath(dir.toString)
        .withXomSrcPaths(Set(xomSourceDir))
        .withAllowUnknown(true)
      b2x.foreach { xml =>
        val b2xFile = dir.resolve("fixture.b2x")
        Files.writeString(b2xFile, xml)
        config = config.withB2xPath(b2xFile.toString)
      }
      val cpg = new Arl2Cpg().createCpg(config).get
      try test(cpg)
      finally cpg.close()
    }

  private def unresolvedFindingJson(cpg: Cpg, call: Call): ujson.Value =
    ujson
      .read(ArlExport.toJson(cpg, "engine-data.cpg"))("findings")
      .arr
      .find(_.obj.get(Keys.CallId).contains(ujson.Str(call.id.toString)))
      .get

  private def unresolvedFinding(cpg: Cpg, call: Call) =
    ArlFindings
      .findings(cpg, Codes.UnresolvedCallTarget)
      .find(finding => ArlFindings.value(finding, Keys.CallId) == call.id().toString)

  private val loanSignature =
    """import loan.Borrower;
      |public signature EngineDataClass extends java.lang.Object {
      |  public in Borrower borrower = null;
      |}
      |""".stripMargin

  "XOM linking of ARL signature types" should {

    "type EngineData in rule, ruleflow, flowtask, and parameterless functiontask bodies" in withCpg(
      Map(
        "engine-data.arl" ->
          s"""$loanSignature
             |ruleset R (EngineDataClass) {
             |  rule `rule.test` {
             |    then {
             |      this.borrower.lastName.equalsIgnoreCase("rule");
             |    }
             |  }
             |}
             |ruleflow `flow`($$EngineData) {
             |  maintask main;
             |}
             |flowtask main($$EngineData) {
             |  {
             |    if ($$EngineData.this.borrower.lastName.equalsIgnoreCase("flow")) {
             |      $$EngineData.this.borrower.lastName.equalsIgnoreCase("flow-body");
             |    }
             |  }
             |}
             |functiontask `main>initResult` {
             |  $$EngineData.this.borrower.lastName.equalsIgnoreCase("function");
             |}
             |""".stripMargin
      )
    ) { cpg =>
      val calls = cpg.call.nameExact("equalsIgnoreCase").l
      calls.size shouldBe 4
      calls.foreach { call =>
        call.methodFullName shouldBe "java.lang.String.equalsIgnoreCase:boolean(java.lang.String)"
        call.signature shouldBe "boolean(java.lang.String)"
        call.typeFullName shouldBe "boolean"
      }
      val flowParameters = cpg.method
        .filter(_.annotation.name("arlKind").parameterAssign.value.code.l.exists(Set("ruleflow", "flowtask")))
        .flatMap(_.parameter.l)
        .filter(_.name == "$EngineData")
        .map(_.typeFullName)
        .toSet
      flowParameters shouldBe Set("EngineDataClass")
      val parameterlessFunction = cpg.method.nameExact("main>initResult").head
      parameterlessFunction.parameter.name.l should not contain "$EngineData"
      parameterlessFunction.annotation.name("arlKind").parameterAssign.value.code.l shouldBe List("functiontask")
    }

    "leave EngineData untyped when multiple ARL signatures are present" in withCpg(
      Map(
        "flow.arl" ->
          s"""$loanSignature
             |flowtask main($$EngineData) {
             |  {
             |    $$EngineData.this.borrower.lastName.equalsIgnoreCase("x");
             |  }
             |}
             |""".stripMargin,
        "other.arl" ->
          """public signature Other extends java.lang.Object {
            |  public in java.lang.String value = null;
            |}
            |""".stripMargin
      )
    ) { cpg =>
      val parameter = cpg.method.nameExact("main").head.parameter.nameExact("$EngineData").head
      parameter.typeFullName shouldBe "ANY"
      cpg.identifier.nameExact("$EngineData").filter(_.typeFullName == "ANY").l should not be empty
      val call    = cpg.call.nameExact("equalsIgnoreCase").head
      val finding = unresolvedFinding(cpg, call).get
      ArlFindings.reason(finding) shouldBe "receiver-unknown"
      ArlFindings.value(finding, Keys.ReceiverType) shouldBe "ANY"
    }

    "resolve contains as synthetic ARL array membership" in withCpg(
      Map(
        "array.arl" ->
          s"""$loanSignature
             |ruleset R (EngineDataClass) {
             |  rule `array.contains` {
             |    then {
             |      new String[]{"IPT", "INP"}.contains(this.borrower.lastName);
             |    }
             |  }
             |}
             |""".stripMargin
      )
    ) { cpg =>
      val contains = cpg.call.nameExact("contains").head
      contains.methodFullName shouldBe "java.lang.String[].contains:boolean(java.lang.String)"
      contains.signature shouldBe "boolean(java.lang.String)"
      contains.typeFullName shouldBe "boolean"
    }

    "report B2X-only methods separately from Java no-candidate calls" in {
      val arlSource =
        """import java.math.BigDecimal;
          |public signature Amounts extends java.lang.Object {
          |  public in BigDecimal amount = null;
          |  public in BigDecimal divisor = null;
          |}
          |ruleset R (Amounts) {
          |  rule `amount.divide` {
          |    then {
          |      amount.divideInternal(divisor);
          |    }
          |  }
          |}
          |""".stripMargin
      val b2xSource =
        """<?xml version="1.0" encoding="UTF-8"?>
          |<b2x:translation xmlns:b2x="http://schemas.ilog.com/JRules/1.3/Translation">
          |  <id>fixture-0000-0000-0000-000000000001</id>
          |  <lang>ARL</lang>
          |  <class>
          |    <businessName>java.math.BigDecimal</businessName>
          |    <method>
          |      <name>divideInternal</name>
          |      <parameter type="java.math.BigDecimal"/>
          |      <body language="arl"><![CDATA[return this;]]></body>
          |    </method>
          |    <method>
          |      <name>divideInternal</name>
          |      <parameter type="java.lang.Object"/>
          |      <body language="arl"><![CDATA[return this;]]></body>
          |    </method>
          |  </class>
          |</b2x:translation>
          |""".stripMargin
      val sourceMap = Map("amount.arl" -> arlSource)

      withCpg(sourceMap, Some(b2xSource)) { cpg =>
        val call    = cpg.call.nameExact("divideInternal").head
        val finding = unresolvedFinding(cpg, call).get
        ArlFindings.reason(finding) shouldBe "bom-only"
        val candidates = List(
          "java.math.BigDecimal.divideInternal(java.lang.Object)",
          "java.math.BigDecimal.divideInternal(java.math.BigDecimal)"
        ).sorted
        ArlFindings.values(finding, Keys.Candidates) shouldBe candidates
        ArlFindings.value(finding, Keys.ReceiverType) shouldBe "java.math.BigDecimal"
        unresolvedFindingJson(cpg, call)(Keys.Candidates).arr.map(_.str).toList shouldBe candidates
      }
      withCpg(sourceMap) { cpg =>
        val call    = cpg.call.nameExact("divideInternal").head
        val finding = unresolvedFinding(cpg, call).get
        ArlFindings.reason(finding) shouldBe "no-candidate"
        ArlFindings.values(finding, Keys.Candidates) shouldBe empty
        ArlFindings.value(finding, Keys.ReceiverType) shouldBe "java.math.BigDecimal"
        unresolvedFindingJson(cpg, call)(Keys.Candidates).arr shouldBe empty
      }
    }
  }
}
