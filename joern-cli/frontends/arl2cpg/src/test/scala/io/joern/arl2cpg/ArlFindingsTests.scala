package io.joern.arl2cpg

import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.{Codes, Keys, Reasons}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArlFindingsTests extends AnyWordSpec with Matchers {

  "ARL finding severities" should {
    "be derived centrally for each known finding code" in {
      val cases = List(
        (Codes.UnknownConstruct, "parser-rule", "error"),
        (Codes.SyntaxError, Codes.SyntaxError, "error"),
        (Codes.UnresolvedCallTarget, "receiver-unknown", "unresolved"),
        (Codes.UnresolvedCallEffects, Reasons.CalleeBodyNotInArtifact, "info"),
        (Codes.UnresolvedCallEffects, Reasons.ReceiverTypeUnknown, "unresolved"),
        (Codes.UnresolvedCallEffects, "another-reason", "unresolved"),
        (Codes.B2xUnmodelledElement, "unmodelled-element", "unresolved"),
        (Codes.BomMember, Codes.BomMember, "info"),
        (Codes.BomTypeUnresolved, Codes.BomTypeUnresolved, "unresolved"),
        (Codes.BomDuplicateClass, Codes.BomDuplicateClass, "unresolved"),
        (Codes.BomIncludeMissing, Codes.BomIncludeMissing, "unresolved"),
        (Codes.BomFilesNotLoaded, Codes.BomFilesNotLoaded, "info")
      )

      cases.foreach { case (code, reason, expectedSeverity) =>
        val finding = ArlFindings.finding(
          Cpg.newDiffGraphBuilder,
          None,
          code,
          reason,
          "message",
          "file.arl",
          None
        )
        finding.keyValuePairs.find(_.key == Keys.Severity).map(_.value) shouldBe Some(expectedSeverity)
      }
      ArlFindings.ListValuedKeys shouldBe Set(Keys.Candidates, Keys.BomFiles)
    }

    "reject unknown finding codes" in {
      intercept[IllegalArgumentException] {
        ArlFindings.finding(
          Cpg.newDiffGraphBuilder,
          None,
          "unexpected-code",
          "reason",
          "message",
          "file.arl",
          None
        )
      }
    }
  }
}
