package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.{Arl2CpgSuite, ArlDefaultTestCpg}
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlTags
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class IntervalTests extends Arl2CpgSuite {

  private def arl(expression: String): String =
    s"""import loan.Borrower;
       |public signature S extends java.lang.Object {
       |  public in Borrower borrower = null;
       |}
       |ruleset R (S) {
       |  rule `r.interval` {
       |    then {
       |      x = $expression;
       |    }
       |  }
       |}
       |""".stripMargin

  private def position(source: String, offset: Int): (Int, Int) = {
    val prefix = source.substring(0, offset)
    (prefix.count(_ == '\n') + 1, prefix.length - prefix.lastIndexOf('\n') - 1)
  }

  private def assertInterval(
    expression: String,
    intervalCode: String,
    lowerClosed: Boolean,
    upperClosed: Boolean,
    lowerCode: String,
    upperCode: String
  ): Call = {
    val source = arl(expression)
    assertInterval(code(source), source, intervalCode, lowerClosed, upperClosed, lowerCode, upperCode)
  }

  private def assertInterval(
    cpg: ArlDefaultTestCpg,
    source: String,
    intervalCode: String,
    lowerClosed: Boolean,
    upperClosed: Boolean,
    lowerCode: String,
    upperCode: String
  ): Call = {
    val interval = cpg.call.name(ArlOperators.interval).l.find(_.code == intervalCode).get
    interval.code shouldBe intervalCode
    interval.tag.nameExact(ArlTags.IntervalLowerClosed).value.l shouldBe List(lowerClosed.toString)
    interval.tag.nameExact(ArlTags.IntervalUpperClosed).value.l shouldBe List(upperClosed.toString)

    interval.argument.size shouldBe 2
    val args = interval.argument.l.sortBy(_.argumentIndex)
    args.map(_.code) shouldBe List(lowerCode, upperCode)

    val startOffset                    = source.indexOf(intervalCode, source.indexOf("x = "))
    val lowerOffset                    = source.indexOf(lowerCode, startOffset + 1)
    val upperOffset                    = source.lastIndexOf(upperCode, startOffset + intervalCode.length)
    val (intervalLine, intervalColumn) = position(source, startOffset)
    val (lowerLine, lowerColumn)       = position(source, lowerOffset)
    val (upperLine, upperColumn)       = position(source, upperOffset)

    interval.lineNumber shouldBe Some(intervalLine)
    interval.columnNumber shouldBe Some(intervalColumn)
    args(0).lineNumber shouldBe Some(lowerLine)
    args(0).columnNumber shouldBe Some(lowerColumn)
    args(1).lineNumber shouldBe Some(upperLine)
    args(1).columnNumber shouldBe Some(upperColumn)
    interval
  }

  "interval literals" should {
    "preserve all lower and upper bracket combinations" in {
      assertInterval("[0,1]", "[0,1]", true, true, "0", "1")
      assertInterval("]0,1]", "]0,1]", false, true, "0", "1")
      assertInterval("[0,1[", "[0,1[", true, false, "0", "1")
      assertInterval("]0,1[", "]0,1[", false, false, "0", "1")
    }

    "preserve dots and unary expressions in bounds" in {
      assertInterval("[0.5,1.5]", "[0.5,1.5]", true, true, "0.5", "1.5")
      assertInterval("[-1, 2.0[", "[-1, 2.0[", true, false, "-1", "2.0")
    }

    "retain expression and method-call bounds" in {
      assertInterval(
        "[borrower.minScore, borrower.maxScore + 1[",
        "[borrower.minScore, borrower.maxScore + 1[",
        lowerClosed = true,
        upperClosed = false,
        lowerCode = "borrower.minScore",
        upperCode = "borrower.maxScore + 1"
      )
      assertInterval(
        "]a.getLow(), 10]",
        "]a.getLow(), 10]",
        lowerClosed = false,
        upperClosed = true,
        lowerCode = "a.getLow()",
        upperCode = "10"
      )
    }

    "preserve whitespace around bounds" in {
      assertInterval("[ 0 , 1 ]", "[ 0 , 1 ]", true, true, "0", "1")
    }

    "keep selectors applied to the interval call" in {
      val source   = arl("[0,1].contains(borrower.creditScore)")
      val cpg      = code(source)
      val interval =
        assertInterval(cpg, source, "[0,1]", lowerClosed = true, upperClosed = true, lowerCode = "0", upperCode = "1")
      val contains = cpg.call.nameExact("contains").head
      contains.code shouldBe "[0,1].contains(borrower.creditScore)"
      contains.receiver.head.id shouldBe interval.id
    }

    "assign distinct columns and bracket tags to same-line intervals" in {
      val source = arl("[0,1] + ]2,3[")
      val cpg    = code(source)
      val first  = assertInterval(cpg, source, "[0,1]", true, true, "0", "1")
      val second = assertInterval(cpg, source, "]2,3[", false, false, "2", "3")
      first.lineNumber shouldBe second.lineNumber
      first.columnNumber should not be second.columnNumber
      first.columnNumber.get should be < second.columnNumber.get
    }
  }
}
