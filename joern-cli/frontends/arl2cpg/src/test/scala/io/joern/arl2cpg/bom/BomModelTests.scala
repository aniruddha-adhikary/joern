package io.joern.arl2cpg.bom

import io.joern.arl2cpg.Main
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.{Codes, Keys}
import io.joern.arl2cpg.passes.resolution.TypeModel
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class BomModelTests extends AnyWordSpec with Matchers {

  private def writeJar(path: Path, entries: List[(String, String)]): Unit = {
    val output = new JarOutputStream(Files.newOutputStream(path))
    try {
      entries.foreach { case (name, content) =>
        output.putNextEntry(new JarEntry(name))
        output.write(content.getBytes(StandardCharsets.UTF_8))
        output.closeEntry()
      }
    } finally output.close()
  }

  "BomModel.load" should {
    "load top-level properties before the package and keep class members" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-top-level-property") { directory =>
        val path = directory.resolve("top-level-property.bom")
        Files.writeString(
          path,
          """property uuid "00000000-0000-0000-0000-000000000001";
            |package loan;
            |public class Foo { void compute(); }
            |""".stripMargin
        )

        val parsed = BomParserFacade.parse(path)
        parsed.properties shouldBe List(BomProperty("uuid", Some("00000000-0000-0000-0000-000000000001")))

        val model = BomModel.load(Nil, Seq(path.toString))
        model.diagnostics shouldBe empty
        model.files.map(_.properties) shouldBe List(parsed.properties)
        model.types.map(_.declaration.fullName) shouldBe List("loan.Foo")
        model.types.head.declaration.members.map(_.name) shouldBe List("compute")
      }
    }

    "accept a top-level property between class declarations" in {
      val parsed = BomParserFacade.parse(
        "inter-class-property.bom",
        """package loan;
          |class Before {}
          |property uuid "first";
          |property version "second";
          |class After {}
          |""".stripMargin
      )

      parsed.properties shouldBe List(BomProperty("uuid", Some("first")), BomProperty("version", Some("second")))
      parsed.types.map(_.name) shouldBe List("Before", "After")
    }

    "load directory files in sorted order and fail closed on duplicates and missing includes" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-directory") { directory =>
        Files.writeString(
          directory.resolve("b.bom"),
          """package sample;
            |class Duplicate { void second(); }
            |""".stripMargin
        )
        Files.writeString(
          directory.resolve("a.bom"),
          """include "missing.bom";
            |package sample;
            |class Duplicate { void first(); }
            |""".stripMargin
        )

        val model = BomModel.load(Nil, Seq(directory.toString))

        model.files.map(_.path) shouldBe List("a.bom", "b.bom")
        model.types shouldBe empty
        model.diagnostics.map(_.code) shouldBe List("bom-include-missing", "bom-duplicate-class")
        model.diagnostics.foreach(_.file should not startWith "/")
        model.diagnostics.last.message should include("a.bom, b.bom")
      }
    }

    "load a symlinked --bom directory" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-symlink") { directory =>
        val bomDirectory = Files.createDirectories(directory.resolve("bom"))
        Files.writeString(bomDirectory.resolve("linked.bom"), "package sample; class Linked {}")
        val symlink = directory.resolve("bom-link")
        Files.createSymbolicLink(symlink, bomDirectory)

        val model = BomModel.load(Nil, Seq(symlink.toString))
        model.types.map(_.declaration.fullName) shouldBe List("sample.Linked")
      }
    }

    "load classpath jars in classpath and entry-name order with relative finding paths" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-classpath") { directory =>
        val firstJar  = directory.resolve("first.jar")
        val secondJar = directory.resolve("second.jar")
        writeJar(
          firstJar,
          List(
            "z/last.bom" -> "package z; class Last {}",
            "a/first.bom" -> "package a; class First {}"
          )
        )
        writeJar(secondJar, List("b/second.bom" -> "include \"missing.bom\"; package b; class Second {}"))

        val model = BomModel.load(
          Seq(firstJar.toString, secondJar.toString),
          Nil,
          Seq("a/first.bom", "z/last.bom", "b/second.bom")
        )

        model.files.map(_.path) shouldBe List(
          "first.jar!/a/first.bom",
          "first.jar!/z/last.bom",
          "second.jar!/b/second.bom"
        )
        model.types.map(_.declaration.fullName) shouldBe List("a.First", "z.Last", "b.Second")
        model.diagnostics.map(_.file) shouldBe List("second.jar!/b/second.bom")
        model.diagnostics.head.file should not startWith "/"
      }
    }

    "load only the default root closure and report other classpath BOM entries" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-roots") { directory =>
        val jar = directory.resolve("roots.jar")
        writeJar(
          jar,
          List(
            "ilog/rules/bom/boot.bom" ->
              """include "com/acme/mid.bom";
                |package sample; class Boot {}
                |""".stripMargin,
            "com/acme/mid.bom" ->
              """include "com/acme/deep/leaf.bom";
                |package sample; class Middle {}
                |""".stripMargin,
            "com/acme/deep/leaf.bom" -> "package sample; class Leaf {}",
            "com/acme/alt-boot.bom"  -> "package sample; class Alternate {}"
          )
        )

        val defaultModel = BomModel.load(Seq(jar.toString), Nil)
        defaultModel.files.map(_.path) shouldBe List(
          "roots.jar!/ilog/rules/bom/boot.bom",
          "roots.jar!/com/acme/mid.bom",
          "roots.jar!/com/acme/deep/leaf.bom"
        )
        defaultModel.types.map(_.declaration.fullName) shouldBe List("sample.Boot", "sample.Middle", "sample.Leaf")
        val defaultSkipped = defaultModel.diagnostics.find(_.code == Codes.BomFilesNotLoaded).get
        defaultSkipped.additionalKeyValues shouldBe List(Keys.BomFiles -> "com/acme/alt-boot.bom")

        val alternateModel = BomModel.load(Seq(jar.toString), Nil, Seq("com/acme/alt-boot.bom"))
        alternateModel.files.map(_.path) shouldBe List("roots.jar!/com/acme/alt-boot.bom")
        alternateModel.types.map(_.declaration.fullName) shouldBe List("sample.Alternate")
        val alternateSkipped = alternateModel.diagnostics.find(_.code == Codes.BomFilesNotLoaded).get
        alternateSkipped.additionalKeyValues shouldBe List(
          Keys.BomFiles -> "com/acme/deep/leaf.bom",
          Keys.BomFiles -> "com/acme/mid.bom",
          Keys.BomFiles -> "ilog/rules/bom/boot.bom"
        )
      }
    }

    "fail closed on duplicate declarations within a selected include closure" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-closure-duplicate") { directory =>
        val jar = directory.resolve("duplicate.jar")
        writeJar(
          jar,
          List(
            "ilog/rules/bom/boot.bom" ->
              """include "ilog/rules/bom/duplicate.bom";
                |package sample; class Duplicate { void fromRoot(); }
                |""".stripMargin,
            "ilog/rules/bom/duplicate.bom" -> "package sample; class Duplicate { void fromInclude(); }"
          )
        )

        val model = BomModel.load(Seq(jar.toString), Nil)
        model.types shouldBe empty
        model.diagnostics.map(_.code) should contain(Codes.BomDuplicateClass)
      }
    }

    "resolve explicit root file paths and report unselected classpath directory entries" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-root-path") { directory =>
        val classpathDir = directory.resolve("classpath")
        val boot = classpathDir.resolve("ilog/rules/bom/boot.bom")
        Files.createDirectories(boot.getParent)
        Files.writeString(boot, "package sample; class Boot {}")
        Files.writeString(classpathDir.resolve("extra.bom"), "package sample; class Extra {}")

        val classpathModel = BomModel.load(Seq(classpathDir.toString), Nil)
        classpathModel.types.map(_.declaration.fullName) shouldBe List("sample.Boot")
        val skipped = classpathModel.diagnostics.find(_.code == Codes.BomFilesNotLoaded).get
        skipped.additionalKeyValues shouldBe List(Keys.BomFiles -> "extra.bom")

        val explicitRoot = directory.resolve("external-root.bom")
        Files.writeString(explicitRoot, "package external; class Root {}")
        val externalModel = BomModel.load(Nil, Nil, Seq(explicitRoot.toString))
        externalModel.types.map(_.declaration.fullName) shouldBe List("external.Root")
      }
    }

    "accept repeatable --bom paths" in {
      Main.parseConfig(Array("--bom", "first.bom", "--bom", "bom-dir")).map(_.bomPaths) shouldBe
        Some(Seq("first.bom", "bom-dir"))
      Main.parseConfig(
        Array("--bom-root", "ilog/rules/bom/boot.bom", "--bom-root", "alternate.bom")
      ).map(_.bomRoots) shouldBe Some(Seq("ilog/rules/bom/boot.bom", "alternate.bom"))
    }

    "retain resolvable BOM members in the linker type model" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-bom-members") { directory =>
        val path = directory.resolve("members.bom")
        Files.writeString(
          path,
          """package ilog.rules.brl;
            |class IlrCollectionUtil {
            |  static boolean isIn(java.util.Collection values, object value);
            |}
            |""".stripMargin
        )
        val model = BomModel.load(Nil, Seq(path.toString))
        val typeModel = new TypeModel(Nil, Nil, model)
        val methods = typeModel.methodsFor("ilog.rules.brl.IlrCollectionUtil", "isIn").map(_.fullName)

        withClue(
          s"diagnostics=${typeModel.bomDiagnostics}; bom=${typeModel.get("ilog.rules.brl.IlrCollectionUtil")}; types=${model.types}"
        ) {
          methods shouldBe List("ilog.rules.brl.IlrCollectionUtil.isIn:boolean(java.util.Collection,java.lang.Object)")
        }
      }
    }
  }
}
