package io.joern.arl2cpg.b2x

import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

/** Unit tests of B2X body analysis on hand-written mappings: what a body does to `this`, and what it honestly cannot
  * account for.
  */
class B2xEffectsTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val Cls                 = "com.acme.T"
  private var tmpDirs: List[Path] = Nil

  override def afterAll(): Unit = tmpDirs.foreach(FileUtil.delete(_, swallowIoExceptions = true))

  private def method(name: String, body: String, params: String*): String =
    s"""<method><name>$name</name>${params.map(p => s"""<parameter type="$p"/>""").mkString}
       |<body language="arl"><![CDATA[$body]]></body></method>""".stripMargin

  private def model(methods: String*): B2xModel = {
    val dir = Files.createTempDirectory("arl2cpg-b2x-effects")
    tmpDirs ::= dir
    val file = dir.resolve("t.b2x")
    Files.writeString(
      file,
      s"""<?xml version="1.0"?><translation><lang>ARL</lang><class><businessName>$Cls</businessName>
         |${methods.mkString("\n")}</class></translation>""".stripMargin
    )
    B2xModel.parse(file)
  }

  private def effectsOf(b2x: B2xModel, name: String): Option[B2xEffects] =
    B2xEffects.of(b2x.candidates(Cls, name, 0).head, b2x)

  "B2X body analysis" should {

    "keep a call on a field of `this` as an opaque effect on that field" in {
      val b2x = model(method("touch", "this.child.modify(); this.child.setName(\"x\");"))
      val fx  = effectsOf(b2x, "touch").get
      fx.opaque shouldBe Set("child.modify")
      fx.inferredWrites shouldBe Set("child.name")
    }

    "not mistake a second call to the same helper for recursion" in {
      val b2x = model(method("twice", "this.update(); this.update();"), method("update", "this.n = this.n + 1;"))
      val fx  = effectsOf(b2x, "twice").get
      fx.opaque shouldBe empty
      fx.writes shouldBe Set("n")
      fx.resolvedThrough.map(_.name) shouldBe List("twice", "update")
    }

    "still stop on genuine recursion" in {
      val b2x = model(method("loop", "this.loop();"))
      effectsOf(b2x, "loop").get.opaque shouldBe Set("loop")
    }

    "follow a mapped accessor body instead of trusting its name" in {
      val b2x = model(
        method("apply", "this.setStatus(1);"),
        method("setStatus", "this.status = s; this.dirty = true; this.audit();", "int")
      )
      val fx = effectsOf(b2x, "apply").get
      fx.inferredWrites shouldBe Set("status")
      fx.writes shouldBe Set("status", "dirty")
      fx.opaque shouldBe Set("audit")
    }

    "treat an unmapped accessor as the inferred property effect only" in {
      val fx = effectsOf(model(method("apply", "this.setStatus(1);")), "apply").get
      fx.inferredWrites shouldBe Set("status")
      fx.opaque shouldBe empty
    }

    "reject a body with trailing garbage rather than read a prefix of it" in {
      B2xEffects.parseBody("this.a = 1; } junk {") shouldBe empty
      B2xEffects.parseBody("this.a = 1; \u0001") shouldBe empty
      B2xEffects.parseBody("this.a = 1;") should not be empty
    }
  }
}
