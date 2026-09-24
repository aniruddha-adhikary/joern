package io.joern.arl2cpg

import io.joern.arl2cpg.parser.ArlParserFacade
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class ParserTests extends AnyWordSpec with Matchers {

  private def writeTmpArl(content: String): Path = {
    val path = Files.createTempFile("arl2cpg-parser-test", ".arl")
    Files.writeString(path, content)
    path
  }

  private def parsesCleanly(content: String): Boolean =
    ArlParserFacade.parse(writeTmpArl(content).toString).toOption.exists(_.errorCount == 0)

  private val corpusDir = Path.of(getClass.getClassLoader.getResource("arl").toURI)

  private val arlFiles =
    Files
      .list(corpusDir)
      .iterator()
      .asScala
      .toList
      .filter(_.toString.endsWith(".arl"))
      .sorted

  "ARL parser" should {

    "find the compiled-ARL corpus on the classpath" in {
      arlFiles should not be empty
    }

    arlFiles.foreach { file =>
      s"parse ${file.getFileName} with 0 syntax errors" in {
        val result = ArlParserFacade.parse(file.toString)
        result.isSuccess shouldBe true
        result.get.errorCount shouldBe 0
        result.get.compilationUnit should not be null
      }
    }

    "parse an 'in out' direction on a signature member" in {
      parsesCleanly("""public signature S extends java.lang.Object {
          |  public in out Integer score = null;
          |}
          |ruleset R (S){ rule `r` { then { } } }
          |""".stripMargin) shouldBe true
    }

    "parse an optional 'import' before an aggregate label" in {
      parsesCleanly("""ruleset R (S){
          |  rule `r` {
          |    when {
          |      import label_a: aggregate { item: Borrower(x > 0); } do { count {item}; }
          |    }
          |    then { }
          |  }
          |}
          |""".stripMargin) shouldBe true
    }

    "report exactly 1 syntax error for a missing ';' and still produce a unit" in {
      val result = ArlParserFacade.parse(writeTmpArl("package x.y; rule Broken { when {} then { insert x } }").toString)
      result.isSuccess shouldBe true
      result.get.errorCount shouldBe 1
      result.get.compilationUnit should not be null
    }

    "parse ',' and 'as' inside a taskNamePart" in {
      parsesCleanly("""ruleset R (S){ rule `r` { then { } } }
          |flowtask body ($p) {
          |  {
          |    call task: body > select x, y as z;
          |  }
          |}
          |""".stripMargin) shouldBe true
    }
  }
}
