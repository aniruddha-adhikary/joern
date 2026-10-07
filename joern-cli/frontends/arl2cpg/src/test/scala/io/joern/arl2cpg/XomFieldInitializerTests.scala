package io.joern.arl2cpg

import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.frontendspecific.arl2cpg.{ArlExport, ArlTags}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class XomFieldInitializerTests extends AnyWordSpec with Matchers {

  private val arlSource =
    """flowtask empty($p) {
      |  {
      |  }
      |}
      |""".stripMargin

  private val javaSource =
    """package sample;
      |
      |public class Holder {
      |  String a = "x\"y";
      |  char c = 'q';
      |  int i = -3;
      |  long l = 5L;
      |  double d = 1.5;
      |  float f = 2f;
      |  boolean b = true;
      |  Object n = null;
      |  Boolean t = Boolean.FALSE;
      |  java.util.List<String> xs = new java.util.ArrayList<>();
      |  int p = 1, q = 2;
      |  static final String K = "153";
      |  int noInit;
      |
      |  class Nested {
      |    int nested = 9;
      |  }
      |}
      |""".stripMargin

  private def withCpg(f: Cpg => Unit): Unit =
    FileUtil.usingTemporaryDirectory("arl2cpg-xom-field-initializers") { dir =>
      Files.writeString(dir.resolve("rules.arl"), arlSource)
      val xomDir = dir.resolve("xom")
      Files.createDirectories(xomDir.resolve("sample"))
      Files.writeString(xomDir.resolve("sample/Holder.java"), javaSource)
      val cpg = new Arl2Cpg()
        .createCpg(
          Config()
            .withInputPath(dir.toString)
            .withXomSrcPaths(Set(xomDir.toString))
            .withAllowUnknown(true)
        )
        .get
      try {
        X2Cpg.applyDefaultOverlays(cpg)
        f(cpg)
      } finally {
        cpg.close()
      }
    }

  private def member(cpg: Cpg, name: String) =
    cpg.typeDecl.fullNameExact("sample.Holder").member.nameExact(name).head

  private def nestedMember(cpg: Cpg) =
    cpg.typeDecl.fullName("sample.Holder.*").member.nameExact("nested").head

  private def tags(member: Member, name: String): Set[String] =
    member.tag.nameExact(name).value.toSet

  "XOM field initializer extraction" should {
    "attach exact initializer and literal metadata to fields" in withCpg { cpg =>
      val expected = Map(
        "a"     -> ("\"x\\\"y\"", "string", Some("x\"y")),
        "c"     -> ("'q'", "char", Some("q")),
        "i"     -> ("-3", "int", Some("-3")),
        "l"     -> ("5L", "long", Some("5")),
        "d"     -> ("1.5", "double", Some("1.5")),
        "f"     -> ("2f", "float", Some("2")),
        "b"     -> ("true", "boolean", Some("true")),
        "n"     -> ("null", "null", None),
        "t"     -> ("Boolean.FALSE", "expression", None),
        "xs"    -> ("new java.util.ArrayList<>()", "expression", None),
        "p"     -> ("1", "int", Some("1")),
        "q"     -> ("2", "int", Some("2")),
        "K"     -> ("\"153\"", "string", Some("153"))
      )
      expected.foreach { case (name, (code, kind, value)) =>
        val field = member(cpg, name)
        tags(field, ArlTags.InitializerCode).shouldBe(Set(code))
        tags(field, ArlTags.LiteralKind).shouldBe(Set(kind))
        tags(field, ArlTags.LiteralValue).shouldBe(value.toSet)
      }

      tags(member(cpg, "noInit"), ArlTags.InitializerCode).shouldBe(empty)
      tags(member(cpg, "noInit"), ArlTags.LiteralKind).shouldBe(empty)
      tags(nestedMember(cpg), ArlTags.InitializerCode).shouldBe(Set("9"))
      tags(nestedMember(cpg), ArlTags.LiteralKind).shouldBe(Set("int"))
      tags(nestedMember(cpg), ArlTags.LiteralValue).shouldBe(Set("9"))
    }

    "export initializer objects additively and deterministically" in withCpg { cpg =>
      val first  = ujson.read(ArlExport.toJson(cpg, "fields.cpg"))
      val second = ujson.read(ArlExport.toJson(cpg, "fields.cpg"))
      ujson.write(first).shouldBe(ujson.write(second))

      val holder = first("types").arr.find(_("fullName").str == "sample.Holder").get
      val memberJson = holder("members").arr.find(_("name").str == "a").get
      memberJson("initializer").obj.keys.toList.shouldBe(List("code", "kind", "value"))
      memberJson("initializer")("code").str.shouldBe("\"x\\\"y\"")
      memberJson("initializer")("kind").str.shouldBe("string")
      memberJson("initializer")("value").str.shouldBe("x\"y")

      val expression = holder("members").arr.find(_("name").str == "xs").get("initializer")
      expression.obj.keys.toList.shouldBe(List("code", "kind"))
      expression("code").str.shouldBe("new java.util.ArrayList<>()")
      expression("kind").str.shouldBe("expression")

      val nullInitializer = holder("members").arr.find(_("name").str == "n").get("initializer")
      nullInitializer.obj.keys.toList.shouldBe(List("code", "kind"))
      nullInitializer("kind").str.shouldBe("null")

      holder("members").arr.find(_("name").str == "noInit").get.obj.contains("initializer").shouldBe(false)
    }
  }
}
