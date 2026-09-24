package io.joern.arl2cpg

import io.joern.arl2cpg.ArlFindings.{Codes, Reasons}
import io.joern.arl2cpg.passes.Gate1Violation
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

  private lazy val withB2x: Cpg    = build(_.withB2xPath(loanB2x.toString))
  private lazy val withoutB2x: Cpg = build(identity)

  override def afterAll(): Unit = {
    withB2x.close()
    withoutB2x.close()
    tmpDirs.foreach(FileUtil.delete(_, swallowIoExceptions = true))
  }

  private def tags(call: Call, name: String): Set[String] = call.tag.nameExact(name).value.toSet

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
      calls(withB2x, "outcome", "rejectWith", 2).map(_.methodFullName).toSet shouldBe Set(method.head.fullName)
      calls(withB2x, "outcome", "rejectWith", 2).foreach(c =>
        tags(c, ArlTags.ResolvesTo) shouldBe Set(method.head.fullName)
      )
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
  }

  "without the mapping" should {

    "report the same call as unresolved, never silently" in {
      // test_without_the_mapping_the_same_call_is_reported_as_unresolved
      writers(withoutB2x, "outcome.rejected") shouldBe empty
      mayAffectReasons(withoutB2x, "rejectWith") shouldBe Set(Reasons.CalleeBodyNotInArtifact)
      withoutB2x.method.fullName(".*:b2x.*").l shouldBe empty
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
