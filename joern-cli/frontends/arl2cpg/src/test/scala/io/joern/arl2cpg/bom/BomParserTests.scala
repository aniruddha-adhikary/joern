package io.joern.arl2cpg.bom

import io.joern.arl2cpg.{Arl2Cpg, Main}
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class BomParserTests extends AnyWordSpec with Matchers {

  private def buildBranchWithBom(bomPath: Path, directory: Path): Unit = {
    val arlPath = directory.resolve("branch.arl")
    Files.writeString(
      arlPath,
      """public signature S extends java.lang.Object {}
        |ruleset R (S) { rule `branch` { when {} then {} } }
        |""".stripMargin
    )
    val config = Main.parseConfig(Array("--bom", bomPath.toString)).get.withInputPath(directory.toString)
    config.bomPaths shouldBe Seq(bomPath.toString)
    val cpg = new Arl2Cpg().createCpg(config).get
    try cpg should not be null
    finally cpg.close()
  }

  "BomParserFacade" should {
    "produce structured types, members, metadata, generics, arrays, and domains" in {
      val parsed = BomParserFacade.parse(
        "memory.bom",
        """
          |include "base.bom";
          |package sample;
          |
          |public class Engine<T extends java.lang.Object>
          |    extends java.lang.Object
          |    implements java.io.Serializable
          |    #valueType
          |    property "label" "engine"
          |{
          |    domain {static ACTIVE, static DISABLED}
          |    public readonly java.util.List<java.lang.String>[] values[] domain 0,* class java.lang.String;
          |    protected java.lang.String @class;
          |    public object object;
          |    Engine(java.lang.String name);
          |    public void accepts(object object)
          |        #factory.default
          |        property default "true";
          |    public boolean before(java.util.Date arg)
          |        property translation.irl "{this}.compareTo({0}) < 0";
          |    public java.lang.String[] find(java.lang.Object... args)
          |        throws java.io.IOException
          |        domain [(int) 0, (int) 12);
          |    public static operator java.lang.String(java.lang.Object value);
          |    interface Nested extends java.io.Serializable {}
          |}
          |
          |class Multi extends java.io.Serializable, java.lang.Cloneable {}
          |""".stripMargin
      )

      parsed.includes shouldBe List("base.bom")
      parsed.types should have size 2
      val engine = parsed.types.find(_.name == "Engine").get
      engine.fullName shouldBe "sample.Engine"
      engine.kind shouldBe BomTypeKind.Class
      engine.modifiers should contain("public")
      engine.typeParameters.map(_.name) shouldBe List("T")
      engine.superClass.map(_.erasedName) shouldBe Some("java.lang.Object")
      engine.interfaces.map(_.erasedName) shouldBe List("java.io.Serializable")
      engine.annotations should contain("valueType")
      engine.properties.map(_.key) should contain("label")
      engine.domains should have size 1
      engine.domains.head shouldBe a[BomDomainSet]

      val values = engine.members.find(_.name == "values").get
      values.kind shouldBe BomMemberKind.Attribute
      values.memberType.map(_.erasedName) shouldBe Some("java.util.List[][]")
      values.domain should not be empty
      values.domain.get shouldBe a[BomCardinalityDomain]

      val escaped = engine.members.find(_.name == "class").get
      escaped.memberType.map(_.erasedName) shouldBe Some("java.lang.String")

      val objectField = engine.members.find(_.name == "object").get
      objectField.memberType.map(_.erasedName) shouldBe Some("java.lang.Object")

      val accepts = engine.members.find(_.name == "accepts").get
      accepts.parameters.map(_.tpe.erasedName) shouldBe List("java.lang.Object")
      accepts.annotations should contain("factory.default")
      accepts.properties.map(_.key) should contain("default")

      val constructor = engine.members.find(_.kind == BomMemberKind.Constructor).get
      constructor.name shouldBe "Engine"
      constructor.parameters.map(_.tpe.erasedName) shouldBe List("java.lang.String")

      val before = engine.members.find(_.name == "before").get
      before.memberType.map(_.erasedName) shouldBe Some("boolean")
      before.properties.map(_.key) should contain("translation.irl")

      val find = engine.members.find(_.name == "find").get
      find.memberType.map(_.erasedName) shouldBe Some("java.lang.String[]")
      find.parameters.head.isVarargs shouldBe true
      find.throwsTypes.map(_.erasedName) shouldBe List("java.io.IOException")
      find.domain should not be empty
      find.domain.get shouldBe a[BomDomainRange]

      engine.members.find(_.kind == BomMemberKind.Operator) shouldBe defined
      engine.nestedTypes.map(_.fullName) shouldBe List("sample.Engine$Nested")
      val multi = parsed.types.find(_.name == "Multi").get
      multi.superClass.map(_.erasedName) shouldBe Some("java.io.Serializable")
      multi.interfaces.map(_.erasedName) shouldBe List("java.lang.Cloneable")
    }

    "preserve Rule Designer directives and parse its property-file BOM" in {
      val parsed = BomParserFacade.parse(
        "borrower.bom",
        """
          |#loadGetterSetterAsProperties
          |
          |property "uuid" "00000000-0000-0000-0000-000000000001";
          |property "version" "1";
          |package loan;
          |public class Borrower { public int creditScore; }
          |""".stripMargin
      )

      parsed.directives shouldBe List("loadGetterSetterAsProperties")
      parsed.properties.map(_.key) shouldBe List("uuid", "version")
      parsed.types.map(_.fullName) shouldBe List("loan.Borrower")
      parsed.types.head.members.find(_.name == "creditScore").flatMap(_.memberType.map(_.erasedName)) shouldBe Some("int")
    }

    "keep top-level directive names in source order" in {
      BomParserFacade.parse("directives.bom", "#firstDirective\n#secondDirective\n").directives shouldBe
        List("firstDirective", "secondDirective")
    }

    "parse contextual BOM keywords used by the domain and Grade repros" in {
      val grade = BomParserFacade.parse(
        "grade.bom",
        """package acme.domain;
          |public class Grade {
          |  static final readonly acme.domain.Grade A;
          |  static final readonly acme.domain.Grade B;
          |}
          |""".stripMargin
      )
      val gradeType = grade.types.head

      gradeType.fullName shouldBe "acme.domain.Grade"
      gradeType.members.map(_.name) shouldBe List("A", "B")
      gradeType.members.map(_.memberType.map(_.erasedName)) shouldBe List(
        Some("acme.domain.Grade"),
        Some("acme.domain.Grade")
      )

      val keywords = BomParserFacade.parse(
        "keywords.bom",
        """package include.domain;
          |class domain {
          |  int domain;
          |  int property;
          |  int operator;
          |  int readonly;
          |  int writeonly;
          |  domain domain(domain domain);
          |}
          |""".stripMargin
      )
      val declaration = keywords.types.head
      declaration.fullName shouldBe "include.domain.domain"
      declaration.members.map(_.name).toSet shouldBe Set("domain", "property", "operator", "readonly", "writeonly")
      val method = declaration.members.find(member => member.kind == BomMemberKind.Method && member.name == "domain").get
      method.memberType.map(_.erasedName) shouldBe Some("domain")
      method.parameters.map(parameter => parameter.tpe.erasedName -> parameter.name) shouldBe List("domain" -> Some("domain"))
    }

    "build branch.arl through the --bom CLI option with either Rule Designer repro" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-rule-designer-cli") { directory =>
        val borrower = directory.resolve("borrower.bom")
        Files.writeString(
          borrower,
          """
            |#loadGetterSetterAsProperties
            |
            |property "uuid" "00000000-0000-0000-0000-000000000001";
            |property "version" "1";
            |package loan;
            |public class Borrower { public int creditScore; }
            |""".stripMargin
        )

        val grade = directory.resolve("grade.bom")
        Files.writeString(
          grade,
          """package acme.domain;
            |public class Grade {
            |  static final readonly acme.domain.Grade A;
            |  static final readonly acme.domain.Grade B;
            |}
            |""".stripMargin
        )

        buildBranchWithBom(borrower, directory)
        buildBranchWithBom(grade, directory)
      }
    }

    "report every lexer and parser syntax error with file, line, and column" in {
      val exception = intercept[BomSyntaxException] {
        BomParserFacade.parse(
          "syntax.bom",
          """package sample;
            |class Broken {
            |  void missing(;
            |  int value
            |""".stripMargin
        )
      }

      exception.errors should not be empty
      exception.errors.foreach { error =>
        error.file shouldBe "syntax.bom"
        error.line should be > 0
        error.column should be >= 0
        error.toString should startWith(s"syntax.bom:${error.line}:${error.column}:")
      }
    }
  }
}
