package io.joern.arl2cpg.bom

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BomParserTests extends AnyWordSpec with Matchers {

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
