package io.joern.arl2cpg

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{Cpg, Properties}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.validation.{PostFrontendValidator, ValidationLevel}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class TypeNodeTests extends AnyWordSpec with Matchers {

  private val arlSource =
    """public signature Rules extends java.lang.Object {
      |  public in first.First first = null;
      |}
      |ruleset R (Rules) {
      |  rule `types` { when {} then {} }
      |}
      |""".stripMargin

  private val firstSource =
    """package first;
      |public class First {
      |  public java.lang.String value;
      |}
      |""".stripMargin

  private val secondSource =
    """package second;
      |public class Second {
      |  public java.lang.Integer value;
      |}
      |""".stripMargin

  private def withCpg(xomSourceSets: Seq[Map[String, String]])(test: Cpg => Unit): Unit =
    FileUtil.usingTemporaryDirectory("arl2cpg-type-node-test") { dir =>
      Files.writeString(dir.resolve("rules.arl"), arlSource)
      val xomDirs = xomSourceSets.zipWithIndex.map { case (sources, index) =>
        val xomDir = dir.resolve(s"xom$index")
        sources.foreach { case (relativePath, source) =>
          val path = xomDir.resolve(relativePath)
          Files.createDirectories(path.getParent)
          Files.writeString(path, source)
        }
        xomDir
      }
      val config = Config()
        .withInputPath(dir.toString)
        .withXomSrcPaths(xomDirs.map(_.toString).toSet)
      val cpg = new Arl2Cpg().createCpg(config).get
      try test(cpg)
      finally cpg.close()
    }

  private def assertUniqueTypes(cpg: Cpg): Unit = {
    val fullNames = cpg.typ.fullName.l
    withClue(s"Duplicate TYPE fullNames: ${fullNames.groupBy(name => name).filter(_._2.size > 1)}") {
      fullNames.distinct.size shouldBe fullNames.size
    }
  }

  private def expectedTypesFromLegacySinglePass(cpg: Cpg): Set[String] = {
    val typeDeclTypes =
      cpg.typeDecl.l.flatMap(typeDecl => typeDecl.fullName :: typeDecl.inheritsFromTypeFullName.toList)
    val typeFullNames =
      cpg.all.map(_.property(Properties.TypeFullName)).filter(_ != null).toList
    (typeDeclTypes ++ typeFullNames :+ Defines.Any)
      .filterNot(_ == "<empty>")
      .filterNot(_.endsWith(NamespaceTraversal.globalNamespaceName))
      .toSet
  }

  "TYPE node creation" should {
    "avoid duplicate full names with one XOM source directory" in withCpg(
      Seq(Map("first/First.java" -> firstSource))
    ) { cpg =>
      assertUniqueTypes(cpg)
      PostFrontendValidator(cpg, ValidationLevel.V3).run()
    }

    "avoid duplicate full names with multiple XOM source directories" in withCpg(
      Seq(
        Map("first/First.java" -> firstSource),
        Map("second/Second.java" -> secondSource)
      )
    ) { cpg =>
      assertUniqueTypes(cpg)
      PostFrontendValidator(cpg, ValidationLevel.V3).run()
    }

    "preserve the TYPE full names from the existing single-pass ARL-only build" in withCpg(Nil) { cpg =>
      assertUniqueTypes(cpg)
      cpg.typ.fullName.toSet shouldBe expectedTypesFromLegacySinglePass(cpg)
    }
  }
}
