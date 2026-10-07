package io.joern.arl2cpg

import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.{Codes, Reasons}
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlTags
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlExport
import io.joern.arl2cpg.b2x.B2xModel
import io.joern.arl2cpg.passes.Gate1Violation
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.validation.{PostFrontendValidator, ValidationLevel}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success}

/** Mirrors the `b2x` section of arlgraph's `tests/test_arlgraph.py` over the same `loan.arl` / `loan.b2x` fixtures.
  *
  * arlgraph answers `data_flow(graph, "outcome.rejected")` with writers / mayAlsoAffect; here the same facts live on
  * the CALL nodes as tags (`ARL_WRITES*`, `ARL_READS*`, `ARL_MAY_AFFECT`) and as FINDING nodes.
  */
class B2xTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val fixtures = Paths.get(getClass.getResource("/b2x").toURI)
  private val loanArl  = fixtures.resolve("loan.arl")
  private val loanB2x  = fixtures.resolve("loan.b2x")

  private var tmpDirs: List[Path] = Nil

  private def build(config: Config => Config): Cpg = {
    val dir = Files.createTempDirectory("arl2cpg-b2x")
    tmpDirs ::= dir
    Files.copy(loanArl, dir.resolve("loan.arl"))
    val cpg = new Arl2Cpg().createCpg(config(Config().withInputPath(dir.toString))).get
    PostFrontendValidator(cpg, ValidationLevel.V3).run()
    cpg
  }

  private val signatureArl =
    """public signature BigDecimalRules extends java.lang.Object {
      |  public in java.math.BigDecimal amount = null;
      |}
      |ruleset BigDecimalRules (BigDecimalRules) {
      |  rule `divide` {
      |    when {}
      |    then { amount.divideInternal(amount); }
      |  }
      |}
      |""".stripMargin

  private def buildWithMapping(
    arl: String,
    mapping: String,
    config: Config => Config = (config: Config) => config
  ): (Cpg, Path) = {
    val dir = Files.createTempDirectory("arl2cpg-b2x-signature")
    tmpDirs ::= dir
    Files.writeString(dir.resolve("signature.arl"), arl)
    val b2xPath = dir.resolve("mapping.b2x")
    Files.writeString(b2xPath, mapping)
    val base = Config().withInputPath(dir.toString).withB2xPath(b2xPath.toString)
    val cpg  = new Arl2Cpg().createCpg(config(base)).get
    PostFrontendValidator(cpg, ValidationLevel.V3).run()
    (cpg, b2xPath)
  }

  private def bigDecimalMapping(returnType: String): String =
    s"""<?xml version="1.0"?>
       |<translation><lang>ARL</lang><class>
       |  <businessName>java.math.BigDecimal</businessName>
       |  <method><name>divideInternal</name>
       |    <parameter type="java.math.BigDecimal"/>
       |    $returnType
       |    <body language="arl"><![CDATA[this.result = value;]]></body>
       |  </method>
       |</class></translation>
       |""".stripMargin

  private val attributeMapping =
    """<?xml version="1.0"?>
      |<translation><lang>ARL</lang><class>
      |  <businessName>com.acme.AttributeSample</businessName>
      |  <attribute><name>stringValue</name><type>java.lang.String</type>
      |    <getter language="arl"><![CDATA[return "153";]]></getter>
      |  </attribute>
      |  <attribute><name>integerValue</name><type>int</type>
      |    <getter language="arl"><![CDATA[return 42;]]></getter>
      |  </attribute>
      |  <attribute><name>negativeDouble</name><type>double</type>
      |    <getter language="arl"><![CDATA[return -1.5;]]></getter>
      |  </attribute>
      |  <attribute><name>booleanValue</name><type>boolean</type>
      |    <getter language="arl"><![CDATA[return true;]]></getter>
      |  </attribute>
      |  <attribute><name>nullValue</name><type>java.lang.Object</type>
      |    <getter language="arl"><![CDATA[return null;]]></getter>
      |  </attribute>
      |  <attribute><name>expressionValue</name><type>java.lang.Integer</type>
      |    <getter language="arl"><![CDATA[return this.getBand();]]></getter>
      |  </attribute>
      |  <attribute><name>missingType</name>
      |    <getter language="arl"><![CDATA[return 7;]]></getter>
      |  </attribute>
      |  <attribute><name>emptyType</name><type/>
      |    <getter language="arl"><![CDATA[return 8;]]></getter>
      |  </attribute>
      |  <attribute><name>duplicateType</name><type>java.lang.Integer</type><type>java.lang.Long</type>
      |    <getter language="arl"><![CDATA[return 9;]]></getter>
      |  </attribute>
      |  <attribute><name>setterOnly</name><type>int</type>
      |    <setter language="arl"><![CDATA[this.band = value;]]></setter>
      |  </attribute>
      |</class></translation>
      |""".stripMargin

  private def divideCall(cpg: Cpg): Call = cpg.call.nameExact("divideInternal").head

  private def findingsForCall(cpg: Cpg, code: String, call: Call) =
    ArlFindings.findings(cpg, code).filter(f => ArlFindings.value(f, ArlFindings.Keys.CallId) == call.id().toString)

  private lazy val withB2x: Cpg    = build(_.withB2xPath(loanB2x.toString))
  private lazy val withoutB2x: Cpg = build(Predef.identity)

  override def afterAll(): Unit = {
    withB2x.close()
    withoutB2x.close()
    tmpDirs.foreach(FileUtil.delete(_, swallowIoExceptions = true))
  }

  private def tags(call: Call, name: String): Set[String] = call.tag.nameExact(name).value.toSet

  private def methodTags(method: Method, name: String): Set[String] = method.tag.nameExact(name).value.toSet

  private def calls(cpg: Cpg, receiver: String, name: String, arity: Int): List[Call] =
    cpg.call.nameExact(name).filter(c => c.argument.size - 1 == arity).filter(_.receiver.code.contains(receiver)).l

  private def writers(cpg: Cpg, path: String): List[Call] =
    cpg.call.filter(c => (tags(c, ArlTags.Writes) ++ tags(c, ArlTags.WritesInferred)).contains(path)).l

  private def mayAffectReasons(cpg: Cpg, callName: String): Set[String] =
    cpg.call.nameExact(callName).flatMap(c => tags(c, ArlTags.MayAffect)).toSet

  "a B2X body" should {

    "turn an opaque call into a real (inferred) write" in {
      // test_b2x_body_turns_an_opaque_call_into_a_real_write
      val rejectWith = calls(withB2x, "outcome", "rejectWith", 2)
      rejectWith should not be empty
      writers(withB2x, "outcome.rejected").map(_.name).toSet shouldBe Set("rejectWith")
      rejectWith.foreach { c =>
        tags(c, ArlTags.WritesInferred) shouldBe Set("outcome.rejected")
        tags(c, ArlTags.Writes) shouldBe empty
      }
    }

    "keep the heuristic and the certain writes apart" in {
      // test_only_certain_drops_the_b2x_heuristics_from_a_field_answer: a consumer that wants only certain
      // writes filters on ARL_WRITES and must still be told the answer is incomplete.
      val certain = withB2x.call.filter(c => tags(c, ArlTags.Writes).contains("outcome.rejected")).l
      certain shouldBe empty
      withB2x.finding.filter(f => ArlFindings.code(f) == Codes.UnresolvedCallEffects).l should not be empty
    }

    "attribute the writer to every rule that calls it" in {
      // test_b2x_writer_is_attributed_to_the_rules_that_call_it
      val callingRules =
        calls(withB2x, "outcome", "rejectWith", 2).flatMap(_.inAst.collectAll[Method].name.headOption).toSet
      callingRules.size should be >= 2
      callingRules should contain("validation.CheckApplicantAge")
    }

    "stay incomplete when the body calls compiled Java" in {
      // test_a_b2x_body_calling_compiled_java_stays_incomplete
      val reasons = mayAffectReasons(withB2x, "rejectWith")
      reasons shouldBe Set(s"${Reasons.B2xBodyCallsMethodWithoutBody}:addReason")
      val findings =
        withB2x.finding.filter(f => ArlFindings.reason(f).startsWith(Reasons.B2xBodyCallsMethodWithoutBody)).l
      findings.map(f => ArlFindings.value(f, ArlFindings.Keys.Line)).toSet.size should be >= 2
    }

    "leave nothing unresolved for a fully resolved body" in {
      // test_a_fully_resolved_body_leaves_nothing_unresolved
      writers(withB2x, "outcome.reported").map(_.name).toSet shouldBe Set("report")
      mayAffectReasons(withB2x, "report") shouldBe empty
    }

    "leave an overloaded method ambiguous rather than guessed" in {
      // test_an_overloaded_method_is_left_ambiguous_rather_than_guessed
      mayAffectReasons(withB2x, "flag") shouldBe Set(Reasons.B2xOverloadAmbiguous)
      writers(withB2x, "outcome.flagged") shouldBe empty
      writers(withB2x, "outcome.flagCode") shouldBe empty
    }

    "resolve to a METHOD node standing for the body" in {
      val method = withB2x.method.fullName(".*rejectWith.*").l
      method.size shouldBe 1
      method.head.filename shouldBe loanB2x.toString
      method.head.code should include("this.setRejected(true)")
      method.head.annotation.name("arlKind").parameterAssign.value.code.l shouldBe List("function")
      val summary = withB2x.method.fullNameExact("com.acme.loan.model.Outcome.summary:ANY()").head
      summary.filename shouldBe loanB2x.toString
      summary.annotation.name("arlKind").parameterAssign.value.code.l shouldBe List("b2x-attribute")
      calls(withB2x, "outcome", "rejectWith", 2).map(_.methodFullName).toSet shouldBe Set(method.head.fullName)
      calls(withB2x, "outcome", "rejectWith", 2).foreach(c =>
        tags(c, ArlTags.ResolvesTo) shouldBe Set(method.head.fullName)
      )
    }

    "use a declared B2X return type in method and call signatures" in {
      val (cpg, b2xPath) =
        buildWithMapping(
          signatureArl,
          bigDecimalMapping("<returnType>java.math.BigDecimal</returnType>")
        )
      try {
        val call   = divideCall(cpg)
        val method = cpg.method.fullNameExact("java.math.BigDecimal.divideInternal:java.math.BigDecimal(java.math.BigDecimal)").head
        val fullName = "java.math.BigDecimal.divideInternal:java.math.BigDecimal(java.math.BigDecimal)"

        call.methodFullName shouldBe fullName
        call.signature shouldBe "java.math.BigDecimal(java.math.BigDecimal)"
        call.typeFullName shouldBe "java.math.BigDecimal"
        method.signature shouldBe "java.math.BigDecimal(java.math.BigDecimal)"
        method.methodReturn.typeFullName shouldBe "java.math.BigDecimal"
        method.filename shouldBe b2xPath.toString

        val finding = findingsForCall(cpg, Codes.B2xMember, call).head
        ArlFindings.value(finding, ArlFindings.Keys.Severity) shouldBe "info"
        ArlFindings.value(finding, "b2xFile") shouldBe b2xPath.toString
        ArlFindings.value(finding, "b2xMember") shouldBe fullName
        ArlFindings.value(finding, "returnTypeSource") shouldBe "b2x"
        finding.evidence.map(_.id).toList should contain(call.id())
        findingsForCall(cpg, Codes.B2xReturnTypeUnknown, call) shouldBe empty
      } finally cpg.close()
    }

    "fall back to the returnType attribute when its element text is empty" in {
      val (cpg, _) = buildWithMapping(signatureArl, bigDecimalMapping("""<returnType type="java.math.BigDecimal"/>"""))
      try {
        val call = divideCall(cpg)
        call.methodFullName shouldBe
          "java.math.BigDecimal.divideInternal:java.math.BigDecimal(java.math.BigDecimal)"
        ArlFindings.value(findingsForCall(cpg, Codes.B2xMember, call).head, "returnTypeSource") shouldBe "b2x"
      } finally cpg.close()
    }

    "reuse a shadowed XOM method while retaining B2X effects" in {
      val xomDir = Files.createTempDirectory("arl2cpg-b2x-signature-xom")
      tmpDirs ::= xomDir
      val source = xomDir.resolve("com/acme/money/Amount.java")
      Files.createDirectories(source.getParent)
      Files.writeString(
        source,
        """package com.acme.money;
          |public class Amount {
          |  public Amount divideInternal(Amount value) { return this; }
          |}
          |""".stripMargin
      )
      val arl =
        """public signature AmountRules extends java.lang.Object {
          |  public in com.acme.money.Amount amount = null;
          |}
          |ruleset AmountRules (AmountRules) {
          |  rule `divide` { when {} then { amount.divideInternal(amount); amount.divideInternal(amount); } }
          |}
          |""".stripMargin
      val mapping =
        """<?xml version="1.0"?>
          |<translation><lang>ARL</lang><class>
          |  <businessName>com.acme.money.Amount</businessName>
          |  <method><name>divideInternal</name>
          |    <parameter type="com.acme.money.Amount"/>
          |    <body language="arl"><![CDATA[this.result = value;]]></body>
          |  </method>
          |</class></translation>
          |""".stripMargin
      val (cpg, b2xPath) =
        buildWithMapping(arl, mapping, _.withXomSrcPaths(Set(xomDir.toString)))
      try {
        val fullName = "com.acme.money.Amount.divideInternal:com.acme.money.Amount(com.acme.money.Amount)"
        val method   = cpg.method.fullNameExact(fullName).head
        val calls    = cpg.call.nameExact("divideInternal").l

        cpg.method.fullNameExact(fullName).size shouldBe 1
        method.filename should endWith("com/acme/money/Amount.java")
        calls.size shouldBe 2
        calls.foreach { call =>
          call.methodFullName shouldBe fullName
          call.signature shouldBe "com.acme.money.Amount(com.acme.money.Amount)"
          call.typeFullName shouldBe "com.acme.money.Amount"
          call.callee(NoResolve).id.l shouldBe List(method.id)
          tags(call, ArlTags.Writes) should contain("amount.result")

          val memberFinding = findingsForCall(cpg, Codes.B2xMember, call).head
          ArlFindings.value(memberFinding, "returnTypeSource") shouldBe "xom"
          val shadowFinding = findingsForCall(cpg, Codes.B2xShadowsMethod, call).head
          ArlFindings.value(shadowFinding, ArlFindings.Keys.Severity) shouldBe "unresolved"
          ArlFindings.value(shadowFinding, "b2xFile") shouldBe b2xPath.toString
          ArlFindings.value(shadowFinding, "b2xMember") shouldBe fullName
          ArlFindings.value(shadowFinding, "shadowedMethodFile") shouldBe method.filename
          ArlFindings.value(shadowFinding, ArlFindings.Keys.Message) should include(
            "replaces the Java method at runtime"
          )
          findingsForCall(cpg, Codes.B2xReturnTypeUnknown, call) shouldBe empty
        }
      } finally cpg.close()
    }

    "fall back to a BOM method return type when the mapping omits one" in {
      val dir = Files.createTempDirectory("arl2cpg-b2x-signature-bom")
      tmpDirs ::= dir
      val bom = dir.resolve("bigdecimal.bom")
      Files.writeString(
        bom,
        """package java.math;
          |class BigDecimal {
          |  java.math.BigDecimal divideInternal(java.math.BigDecimal value);
          |}
          |""".stripMargin
      )
      val (cpg, _) =
        buildWithMapping(signatureArl, bigDecimalMapping(""), _.withBomPaths(Seq(bom.toString)))
      try {
        val call = divideCall(cpg)
        call.methodFullName shouldBe
          "java.math.BigDecimal.divideInternal:java.math.BigDecimal(java.math.BigDecimal)"
        call.signature shouldBe "java.math.BigDecimal(java.math.BigDecimal)"
        call.typeFullName shouldBe "java.math.BigDecimal"
        ArlFindings.value(findingsForCall(cpg, Codes.B2xMember, call).head, "returnTypeSource") shouldBe "bom"
        findingsForCall(cpg, Codes.B2xReturnTypeUnknown, call) shouldBe empty
      } finally cpg.close()
    }

    "report empty and duplicate returnType elements as unmodelled" in {
      val duplicate = "<returnType>java.math.BigDecimal</returnType>"
      val (cpg, _) =
        buildWithMapping(signatureArl, bigDecimalMapping(s"<returnType/>$duplicate"), _.withAllowUnknown(true))
      try {
        val unmodelled = ArlFindings.findings(cpg, Codes.B2xUnmodelledElement)
        unmodelled.map(ArlFindings.reason).toSet shouldBe
          Set("method/returnType-empty", "method/returnType-duplicate")
        val call = divideCall(cpg)
        ArlFindings.value(findingsForCall(cpg, Codes.B2xMember, call).head, "returnTypeSource") shouldBe "none"
        findingsForCall(cpg, Codes.B2xReturnTypeUnknown, call) should have size 1
      } finally cpg.close()
    }

    "report calls whose B2X method has no known return type" in {
      val callsWithoutType = calls(withB2x, "outcome", "rejectWith", 2)
      callsWithoutType should not be empty
      val callIds         = callsWithoutType.map(_.id().toString).toSet
      val memberFindings  =
        ArlFindings.findings(withB2x, Codes.B2xMember)
          .filter(f => callIds.contains(ArlFindings.value(f, ArlFindings.Keys.CallId)))
      val returnFindings =
        ArlFindings.findings(withB2x, Codes.B2xReturnTypeUnknown)
          .filter(f => callIds.contains(ArlFindings.value(f, ArlFindings.Keys.CallId)))
      memberFindings.size shouldBe callsWithoutType.size
      returnFindings.size shouldBe callsWithoutType.size
      callsWithoutType.foreach { call =>
        call.methodFullName shouldBe
          "com.acme.loan.model.Outcome.rejectWith:ANY(com.acme.loan.model.Reason,int)"
        call.signature shouldBe "ANY(com.acme.loan.model.Reason,int)"
        call.typeFullName shouldBe "ANY"
        val memberFinding = findingsForCall(withB2x, Codes.B2xMember, call).head
        ArlFindings.value(memberFinding, ArlFindings.Keys.Severity) shouldBe "info"
        ArlFindings.value(memberFinding, "returnTypeSource") shouldBe "none"
        val returnTypeFinding = findingsForCall(withB2x, Codes.B2xReturnTypeUnknown, call).head
        ArlFindings.value(returnTypeFinding, ArlFindings.Keys.Severity) shouldBe "unresolved"
        ArlFindings.value(returnTypeFinding, "b2xFile") shouldBe loanB2x.toString
        ArlFindings.value(returnTypeFinding, "b2xMember") shouldBe call.methodFullName
        ArlFindings.value(returnTypeFinding, "returnTypeSource") shouldBe "none"
        ArlFindings.value(returnTypeFinding, ArlFindings.Keys.Message) should include(
          "com.acme.loan.model.Outcome.rejectWith(com.acme.loan.model.Reason,int)"
        )
      }
    }

    "create and export typed B2X attribute getter methods with literal metadata" in {
      val (cpg, b2xPath) =
        buildWithMapping(signatureArl, attributeMapping, _.withAllowUnknown(true))
      try {
        val expectedFullNames = Map(
          "stringValue"     -> "com.acme.AttributeSample.stringValue:java.lang.String()",
          "integerValue"    -> "com.acme.AttributeSample.integerValue:int()",
          "negativeDouble"  -> "com.acme.AttributeSample.negativeDouble:double()",
          "booleanValue"    -> "com.acme.AttributeSample.booleanValue:boolean()",
          "nullValue"       -> "com.acme.AttributeSample.nullValue:java.lang.Object()",
          "expressionValue" -> "com.acme.AttributeSample.expressionValue:java.lang.Integer()",
          "missingType"     -> "com.acme.AttributeSample.missingType:ANY()",
          "emptyType"       -> "com.acme.AttributeSample.emptyType:ANY()",
          "duplicateType"  -> "com.acme.AttributeSample.duplicateType:ANY()"
        )
        expectedFullNames.foreach { case (name, fullName) =>
          val method = cpg.method.fullNameExact(fullName).head
          method.name shouldBe name
          method.signature shouldBe fullName.split(":").last
          method.methodReturn.typeFullName shouldBe fullName.split(":").last.stripSuffix("()")
          method.filename shouldBe b2xPath.toString
          method.annotation.name("arlKind").parameterAssign.value.code.l shouldBe List("b2x-attribute")
          methodTags(method, ArlTags.B2xAttribute) shouldBe Set(b2xPath.toString)
        }
        cpg.method.nameExact("setterOnly").l shouldBe empty

        val model  = B2xModel.parse(b2xPath)
        val setter = model.members.find(_.kind == "setter").get
        setter.paramTypes shouldBe List("int")
        setter.returnType shouldBe None
        model.candidates("com.acme.AttributeSample", "stringValue", 0) shouldBe empty

        val stringMethod = cpg.method.fullNameExact(expectedFullNames("stringValue")).head
        methodTags(stringMethod, ArlTags.LiteralKind) shouldBe Set("string")
        methodTags(stringMethod, ArlTags.LiteralValue) shouldBe Set("153")
        val intMethod = cpg.method.fullNameExact(expectedFullNames("integerValue")).head
        methodTags(intMethod, ArlTags.LiteralKind) shouldBe Set("int")
        methodTags(intMethod, ArlTags.LiteralValue) shouldBe Set("42")
        val negativeMethod = cpg.method.fullNameExact(expectedFullNames("negativeDouble")).head
        methodTags(negativeMethod, ArlTags.LiteralKind) shouldBe Set("double")
        methodTags(negativeMethod, ArlTags.LiteralValue) shouldBe Set("-1.5")
        val booleanMethod = cpg.method.fullNameExact(expectedFullNames("booleanValue")).head
        methodTags(booleanMethod, ArlTags.LiteralKind) shouldBe Set("boolean")
        methodTags(booleanMethod, ArlTags.LiteralValue) shouldBe Set("true")
        val nullMethod = cpg.method.fullNameExact(expectedFullNames("nullValue")).head
        methodTags(nullMethod, ArlTags.LiteralKind) shouldBe Set("null")
        methodTags(nullMethod, ArlTags.LiteralValue) shouldBe empty
        val expressionMethod = cpg.method.fullNameExact(expectedFullNames("expressionValue")).head
        methodTags(expressionMethod, ArlTags.LiteralKind) shouldBe empty
        methodTags(expressionMethod, ArlTags.LiteralValue) shouldBe empty

        val json       = ujson.read(ArlExport.toJson(cpg, "attributes.cpg"))
        val attributes = json("b2xAttributes").arr
        attributes.map(_("name").str).toList shouldBe expectedFullNames.keys.toList.sorted
        attributes.foreach(attribute => attribute("shadowsMethod").bool shouldBe false)
        val byName = attributes.map(attribute => attribute("name").str -> attribute).toMap
        byName("stringValue")("literal").obj.toMap shouldBe Map(
          "kind"  -> ujson.Str("string"),
          "value" -> ujson.Str("153")
        )
        byName("integerValue")("literal").obj.toMap shouldBe Map(
          "kind"  -> ujson.Str("int"),
          "value" -> ujson.Str("42")
        )
        byName("negativeDouble")("literal").obj.toMap shouldBe Map(
          "kind"  -> ujson.Str("double"),
          "value" -> ujson.Str("-1.5")
        )
        byName("booleanValue")("literal").obj.toMap shouldBe Map(
          "kind"  -> ujson.Str("boolean"),
          "value" -> ujson.Str("true")
        )
        byName("nullValue")("literal").obj.toMap shouldBe Map("kind" -> ujson.Str("null"))
        byName("expressionValue").obj.contains("literal") shouldBe false
        expectedFullNames.foreach { case (name, fullName) =>
          val methodJson = json("methods").arr.find(_("fullName").str == fullName).get
          byName(name)("methodStableId").str shouldBe methodJson("stableId").str
        }

        val missingTypeMethod = cpg.method.fullNameExact(expectedFullNames("missingType")).head
        val unknownFinding    =
          ArlFindings.findings(cpg, Codes.B2xReturnTypeUnknown)
            .find(finding => ArlFindings.value(finding, "b2xMember") == missingTypeMethod.fullName)
            .get
        ArlFindings.value(unknownFinding, ArlFindings.Keys.Severity) shouldBe "unresolved"
        ArlFindings.value(unknownFinding, "b2xFile") shouldBe b2xPath.toString
        ArlFindings.value(unknownFinding, ArlFindings.Keys.CallId) shouldBe ""
        unknownFinding.evidence.map(_.id).toList shouldBe List(missingTypeMethod.id)
        ArlFindings.findings(cpg, Codes.B2xUnmodelledElement)
          .map(ArlFindings.reason)
          .toSet should contain allOf ("attribute/type-empty", "attribute/type-duplicate")
      } finally cpg.close()
    }

    "reuse a colliding Java getter METHOD and report the B2X attribute shadow" in {
      val xomDir = Files.createTempDirectory("arl2cpg-b2x-attribute-xom")
      tmpDirs ::= xomDir
      val source = xomDir.resolve("com/acme/AttributeSample.java")
      Files.createDirectories(source.getParent)
      Files.writeString(
        source,
        """package com.acme;
          |public class AttributeSample {
          |  public String stringValue() { return "java"; }
          |}
          |""".stripMargin
      )
      val (cpg, b2xPath) =
        buildWithMapping(
          signatureArl,
          attributeMapping,
          _.withXomSrcPaths(Set(xomDir.toString)).withAllowUnknown(true)
        )
      try {
        val fullName = "com.acme.AttributeSample.stringValue:java.lang.String()"
        val method   = cpg.method.fullNameExact(fullName).head
        cpg.method.fullNameExact(fullName).size shouldBe 1
        method.filename should endWith("com/acme/AttributeSample.java")
        method.annotation.name("arlKind").parameterAssign.value.code.l shouldBe List("xom")
        methodTags(method, ArlTags.B2xAttribute) shouldBe Set(b2xPath.toString)
        methodTags(method, ArlTags.LiteralKind) shouldBe Set("string")
        methodTags(method, ArlTags.LiteralValue) shouldBe Set("153")

        val shadow = ArlFindings.findings(cpg, Codes.B2xShadowsMethod)
          .find(finding => ArlFindings.value(finding, "b2xMember") == fullName)
          .get
        ArlFindings.value(shadow, ArlFindings.Keys.Severity) shouldBe "unresolved"
        ArlFindings.value(shadow, "b2xFile") shouldBe b2xPath.toString
        ArlFindings.value(shadow, "shadowedMethodFile") shouldBe method.filename
        ArlFindings.value(shadow, ArlFindings.Keys.CallId) shouldBe ""
        shadow.evidence.map(_.id).toList shouldBe List(method.id)

        val json      = ujson.read(ArlExport.toJson(cpg, "attribute-shadow.cpg"))
        val attribute = json("b2xAttributes").arr.find(_("name").str == "stringValue").get
        val methodJson = json("methods").arr.find(_("fullName").str == fullName).get
        methodJson("arlKind").str shouldBe "xom"
        attribute("methodStableId").str shouldBe
          methodJson("stableId").str
        attribute("shadowsMethod").bool shouldBe true
        attribute("literal")("value").str shouldBe "153"
      } finally cpg.close()
    }

    "connect normal completion in B2X function bodies to METHOD_RETURN" in {
      val cpg = build(_.withB2xPath(loanB2x.toString))
      try {
        X2Cpg.applyDefaultOverlays(cpg)
        val method = cpg.method.fullName(".*rejectWith.*").head
        method.methodReturn.cfgIn.l.exists(_.method.id == method.id) shouldBe true
      } finally cpg.close()
    }

    "say so when a class is absent from the mapping" in {
      // test_a_class_absent_from_the_mapping_says_so
      val reasons = withB2x.finding.map(ArlFindings.reason).toSet
      reasons.intersect(Set(Reasons.ClassNotInB2x, Reasons.ReceiverTypeUnknown)) should not be empty
    }

    "without the type model leave the inherited body unresolved" in {
      // test_without_the_type_model_the_inherited_body_stays_unresolved
      writers(withB2x, "outcome.archived") shouldBe empty
      mayAffectReasons(withB2x, "archive") shouldBe Set(Reasons.NoB2xBodyForMethod)
    }

    "see through a cast on the receiver" in {
      val dir = Files.createTempDirectory("arl2cpg-b2x-cast")
      tmpDirs ::= dir
      Files.writeString(
        dir.resolve("cast.arl"),
        """public signature LoanValidation extends ilog.rules.engine.IlrSignature {
          |    public out com.acme.loan.model.Outcome outcome = null;
          |}
          |ruleset LoanValidation (LoanValidation) {
          |  rule Cast {
          |    when {} then { ((com.acme.loan.model.Outcome) outcome).report(); }
          |  }
          |}
          |""".stripMargin
      )
      val cpg = new Arl2Cpg().createCpg(Config().withInputPath(dir.toString).withB2xPath(loanB2x.toString)).get
      try {
        val report = cpg.call.nameExact("report").l
        report.size shouldBe 1
        tags(report.head, ArlTags.WritesInferred) shouldBe Set("(com.acme.loan.model.Outcome) outcome.reported")
        tags(report.head, ArlTags.MayAffect) shouldBe empty
      } finally cpg.close()
    }
  }

  "without the mapping" should {

    "report the same call as unresolved, never silently" in {
      // test_without_the_mapping_the_same_call_is_reported_as_unresolved
      writers(withoutB2x, "outcome.rejected") shouldBe empty
      mayAffectReasons(withoutB2x, "rejectWith") shouldBe Set(Reasons.CalleeBodyNotInArtifact)
      withoutB2x.method.filter(_.filename == loanB2x.toString).l shouldBe empty
    }
  }

  "the --b2x option" should {

    "fail instead of downgrading the graph when the file is missing" in {
      // test_a_missing_b2x_file_fails_instead_of_downgrading_the_graph
      val dir = Files.createTempDirectory("arl2cpg-b2x-missing")
      tmpDirs ::= dir
      Files.copy(loanArl, dir.resolve("loan.arl"))
      val config = Config().withInputPath(dir.toString).withB2xPath(dir.resolve("nope").toString)
      new Arl2Cpg().createCpg(config) match {
        case Failure(e)   => e.getMessage should include("nope")
        case Success(cpg) => cpg.close(); fail("a missing mapping must not produce a graph")
      }
    }

    "be as loud about an unmodelled b2x element as about an unmodelled ARL construct" in {
      // test_an_unmodelled_b2x_element_is_as_loud_as_an_unmodelled_arl_construct
      val dir = Files.createTempDirectory("arl2cpg-b2x-odd")
      tmpDirs ::= dir
      Files.copy(loanArl, dir.resolve("loan.arl"))
      val odd = dir.resolve("odd.b2x")
      Files.writeString(
        odd,
        """<?xml version="1.0"?>
          |<translation><lang>ARL</lang><class><businessName>com.acme.loan.model.Outcome</businessName>
          |<somethingNew>x</somethingNew></class></translation>
          |""".stripMargin
      )
      val base = Config().withInputPath(dir.toString).withB2xPath(odd.toString)
      new Arl2Cpg().createCpg(base) match {
        case Failure(e: Gate1Violation) => e.unmodelledB2xElements shouldBe 1
        case Failure(e)                 => fail(e)
        case Success(cpg)               => cpg.close(); fail("Gate 1 must reject an unmodelled mapping element")
      }
      val cpg = new Arl2Cpg().createCpg(base.withAllowUnknown(true)).get
      try {
        val findings = cpg.finding.filter(f => ArlFindings.code(f) == Codes.B2xUnmodelledElement).l
        findings.size shouldBe 1
        ArlFindings.value(findings.head, ArlFindings.Keys.Message) should include("somethingNew")
      } finally cpg.close()
    }
  }
}
