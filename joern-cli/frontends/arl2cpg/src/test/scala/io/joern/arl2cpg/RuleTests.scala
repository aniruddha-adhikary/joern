package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.Files

class RuleTests extends Arl2CpgSuite {

  private val baseArl = """import loan.Borrower;
import loan.LoanRequest;
import loan.Report;
public signature EngineDataClass extends com.ibm.rules.engine.runtime.impl.AbstractEngineData {
  public in Borrower borrower = null;
  public out String grade = "";
  public out Integer score = null;
}

ruleset IlrContext (EngineDataClass){
  rule `chain.a_reads_after` {
    property priority = 10;
    ilog.rules.business_name = "a_reads_after"
    status = "new"
    when {
      Report() from report;
      evaluate ( report.approved);
    }
    then {
      score = Integer.valueOf((int)(score.intValue() + 100));
      updateEngineData;
    }
  }
}
"""

  "a rule" should {

    val cpg = code(baseArl)

    "lower to a METHOD on the ruleset TYPE_DECL" in {
      val rule = cpg.method.name("chain.a_reads_after").head
      rule.fullName shouldBe "IlrContext.chain.a_reads_after:void()"
      rule.signature shouldBe "void()"
      rule.astParentFullName shouldBe "IlrContext"
      rule.methodReturn.typeFullName shouldBe "void"
    }

    "carry property annotations" in {
      val ann = cpg.method.name("chain.a_reads_after").annotation.l
      ann.map(_.name) should contain allOf ("priority", "ilog.rules.business_name", "status")
    }

    "lower the when-pattern to a LOCAL + cast assignment + IF condition" in {
      val rule = cpg.method.name("chain.a_reads_after").head
      // anonymous Report() from report gets a synthetic binding
      val locals = rule.block.local.name.l
      locals.exists(_.startsWith("$Report_")) shouldBe true
      val casts = rule.call.name(Operators.cast).l
      casts.size should be >= 1
      rule.controlStructure.headOption should not be empty
      rule.controlStructure.head.controlStructureType shouldBe "IF"
    }
  }

  "a signature" should {

    val cpg = code(baseArl)

    "lower to a TYPE_DECL with MEMBERs and direction annotations" in {
      val sig = cpg.typeDecl.name("EngineDataClass").head
      sig.inheritsFromTypeFullName shouldBe List("com.ibm.rules.engine.runtime.impl.AbstractEngineData")
      val member = sig.member.name("borrower").head
      member.typeFullName shouldBe "loan.Borrower"
      sig.member.name("borrower").annotation.name.l shouldBe List("direction")
    }
  }

  "when-statement variants" should {

    "lower exists to <operator>.exists" in {
      val cpg = code("""ruleset R (S){
          |  rule `r.exists` {
          |    when {
          |      exists { Borrower(x > 0); }
          |    }
          |    then { }
          |  }
          |}
          |""".stripMargin)
      cpg.call.name(ArlOperators.exists).size shouldBe 1
    }

    "lower not { ... } to logicalNot(exists(...))" in {
      val cpg = code("""ruleset R (S){
          |  rule `r.not` {
          |    when {
          |      not { Borrower(x > 0); }
          |    }
          |    then { }
          |  }
          |}
          |""".stripMargin)
      cpg.call.name(Operators.logicalNot).size shouldBe 1
      cpg.call.name(ArlOperators.exists).size shouldBe 1
    }

    "lower an aggregate to <operator>.aggregate" in {
      val cpg = code("""ruleset R (S){
          |  rule `r.agg` {
          |    when {
          |      count_label: aggregate {
          |        item: Borrower(x > 0);
          |      } do {
          |        count {item};
          |      }
          |    }
          |    then { }
          |  }
          |}
          |""".stripMargin)
      val agg = cpg.call.name(ArlOperators.aggregate).head
      agg.code should include("aggregate")
      cpg.local.name("count_label").headOption should not be empty
      cpg.local.name("count_label").head.typeFullName shouldBe "int"
    }

    "lower collect from sources as casts and retain its filter" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-aggregate-collect-from") { directory =>
        val xomDir  = directory.resolve("xom")
        val sources = Map(
          "acme/Order.java" ->
            """package acme;
              |public class Order { public java.util.List<Line> lines; }
              |""".stripMargin,
          "acme/Line.java" ->
            """package acme;
              |public class Line { public Integer sequenceNumber; }
              |""".stripMargin,
          "acme/Result.java" ->
            """package acme;
              |public class Result {}
              |""".stripMargin,
          "ilog/rules/brl/IlrCollectionUtil.java" ->
            """package ilog.rules.brl;
              |public class IlrCollectionUtil {
              |  public static int getSize(java.util.Collection values) { return values.size(); }
              |}
              |""".stripMargin
        )
        sources.foreach { case (relativePath, source) =>
          val path = xomDir.resolve(relativePath)
          Files.createDirectories(path.getParent)
          Files.writeString(path, source)
        }

        val source =
          """import acme.Order;
            |import acme.Line;
            |import acme.Result;
            |import java.util.ArrayList;
            |import ilog.rules.brl.IlrCollectionUtil;
            |public signature S extends java.lang.Object {}
            |ruleset R (S) {
            |  rule `aggregate.from` {
            |    when {
            |      o : Order();
            |      numberedLines:aggregate {
            |        collect_class_1 : Line(sequenceNumber.intValue() > 0) from o.lines;
            |      } do {
            |        ArrayList<Line>{collect_class_1};
            |      }
            |      r : Result();
            |    }
            |    then {
            |      System.out.println(IlrCollectionUtil.getSize(numberedLines));
            |    }
            |  }
            |}
            |""".stripMargin
        val inputDir = directory.resolve("input")
        Files.createDirectories(inputDir)
        Files.writeString(inputDir.resolve("aggregate.arl"), source)
        val config = Config()
          .withInputPath(inputDir.toString)
          .withXomSrcPaths(Set(xomDir.toString))
          .withAllowUnknown(true)
        val cpg = new Arl2Cpg().createCpg(config).get
        try {
          val collectLocal = cpg.local.nameExact("collect_class_1").head
          collectLocal.typeFullName shouldBe "acme.Line"
          val labelLocal = cpg.local.nameExact("numberedLines").head
          labelLocal.typeFullName shouldBe "java.util.ArrayList"
          cpg.identifier.nameExact("numberedLines").l.map(_.typeFullName).distinct shouldBe
            List("java.util.ArrayList")

          val bindingAssignment =
            cpg.call.name(Operators.assignment).find(_.argument.l.exists(_.code == "collect_class_1")).get
          bindingAssignment.argument.l.find(_.argumentIndex == 2).get match {
            case cast: Call => cast.name shouldBe Operators.cast
            case other      => fail(s"Expected collect assignment RHS to be a cast, got $other")
          }

          val aggregateCall = cpg.call.name(ArlOperators.aggregate).head
          aggregateCall.argument.l.exists(_.code.contains("sequenceNumber.intValue() > 0")) shouldBe true

          val getSize = cpg.call.nameExact("getSize").head
          getSize.methodFullName shouldBe "ilog.rules.brl.IlrCollectionUtil.getSize:int(java.util.Collection)"
          ArlFindings
            .findings(cpg, ArlFindings.Codes.UnresolvedCallTarget)
            .filter(f => ArlFindings.value(f, ArlFindings.Keys.CallId) == getSize.id().toString) shouldBe empty
        } finally cpg.close()
      }
    }

    "lower evaluate with binding to a local + assignment + conjuncts" in {
      val cpg = code("""ruleset R (S){
          |  rule `r.eval` {
          |    when {
          |      evaluate ( v : borrower.creditScore; v > 100 );
          |    }
          |    then { }
          |  }
          |}
          |""".stripMargin)
      cpg.local.name("v").headOption should not be empty
    }
  }

  "a syntactically broken rule" should {

    "fail the build by default (Gate 1: never silently drop)" in {
      val ex = intercept[io.joern.arl2cpg.passes.Gate1Violation] {
        code("package x.y; rule Broken { when {} then { insert x } }").method.l
      }
      ex.filesWithSyntaxErrors shouldBe 1
    }

    "with --allow-unknown still produce a CPG containing the rule METHOD, plus a syntax-error FINDING" in {
      val cpg =
        code("package x.y; rule Broken { when {} then { insert x } }").withConfig(Config().withAllowUnknown(true))
      cpg.method.name("Broken").headOption should not be empty
      val syntax = cpg.finding.filter(f => ArlFindings.code(f) == ArlFindings.Codes.SyntaxError).l
      syntax.size shouldBe 1
      ArlFindings.value(syntax.head, ArlFindings.Keys.Filename) should endWith("test.arl")
    }
  }

  "match many" should {

    "produce one IF per case plus a negated default IF" in {
      val cpg = code("""ruleset R (S){
          |  rule `r.match` {
          |    when { }
          |    match many {
          |      case (x == 1) : then 1{ }
          |      case (x == 2) : then 2{ }
          |      default : then 3{ }
          |    }
          |  }
          |}
          |""".stripMargin)
      val ifs = cpg.method.name("r.match").controlStructure.controlStructureType("IF").l
      ifs.size shouldBe 3
      cpg.call.name(Operators.logicalNot).size shouldBe 1
      cpg.block.code("match many").size shouldBe 1
      cpg.block.code("then 1").size shouldBe 1
    }
  }
}
