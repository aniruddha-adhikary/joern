package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.Defines

class ExpressionTests extends Arl2CpgSuite {

  private val arl = """import loan.Borrower;
public signature S extends java.lang.Object {
  public in Borrower borrower = null;
  public out Integer score = null;
}
ruleset R (S){
  rule `r.expr` {
    then {
      score = Integer.valueOf((int)(score.intValue() + 100));
      updateEngineData;
      insert borrower;
      borrower = borrower as Borrower;
    }
  }
}
"""

  val cpg = code(arl)

  "signature parameters" should {
    "lower to this.x fieldAccess" in {
      val accesses = cpg.call.name(Operators.fieldAccess).code(".*borrower.*").l
      accesses.size should be > 0
      val fa = accesses.head
      fa.argument.l.exists(_.code == "borrower") shouldBe true
    }
  }

  "static calls" should {
    "resolve Integer.valueOf to java.lang.Integer.valueOf" in {
      val call = cpg.call.name("valueOf").head
      call.methodFullName shouldBe s"java.lang.Integer.valueOf:${Defines.UnresolvedSignature}(1)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }

  "casts" should {
    "lower (int)(e) to <operator>.cast" in {
      cpg.call.name(Operators.cast).size should be >= 1
    }
    "lower `e as T` to <operator>.cast" in {
      val casts = cpg.call.name(Operators.cast).l
      casts.exists(_.code.contains("as") || casts.size >= 2) shouldBe true
    }
  }

  "intervals" should {
    "lower [lo,hi] to <operator>.interval" in {
      val cpg2 = code("""ruleset R2 (S2){
          |  rule `r.int` {
          |    then {
          |      x = [0,1].contains(borrower.creditScore);
          |    }
          |  }
          |}
          |""".stripMargin)
      cpg2.call.name(ArlOperators.interval).size shouldBe 1
    }
  }

  "bare identifier statements" should {
    "lower updateEngineData; to an identifier expression" in {
      val ident = cpg.identifier.name("updateEngineData").headOption
      ident should not be empty
    }
    "lower `insert x;` to CALL insert" in {
      val call = cpg.call.name("insert").headOption
      call should not be empty
      call.get.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }
}
