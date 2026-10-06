package io.joern.arl2cpg

import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

class ResolutionRegressionTests extends AnyWordSpec with Matchers {

  "the pre-linker resolution baseline" should {
    "keep every golden call at the same target" in {
      val arlDir    = Paths.get(getClass.getResource("/arl").toURI).toAbsolutePath.normalize()
      val xomSrcDir = Paths.get(getClass.getResource("/xom").toURI).toString
      val golden    = Files
        .readAllLines(Paths.get(getClass.getResource("/golden/resolved-calls.tsv").toURI))
        .asScala
        .drop(1)
        .toSet
      val cpg = new Arl2Cpg()
        .createCpg(
          Config()
            .withInputPath(arlDir.toString)
            .withXomSrcPaths(Set(xomSrcDir))
            .withAllowUnknown(true)
        )
        .get
      try {
        val actual  = ResolutionCoverageRunner.resolvedCallRows(cpg, arlDir).map(_.tsv).toSet
        val missing = golden.diff(actual)
        withClue(s"Golden rows no longer resolve exactly as before:\n${missing.toList.sorted.mkString("\n")}\n") {
          missing shouldBe empty
        }
      } finally cpg.close()
    }
  }
}
