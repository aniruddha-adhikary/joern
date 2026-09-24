package io.joern.arl2cpg

import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class BindingTests extends Arl2CpgSuite {

  private val arl = """public signature S extends java.lang.Object {
  public in Borrower borrower = null;
}
ruleset R (S){
  rule `bound.rule` {
    when {
      b: Borrower();
      evaluate ( b.age > 18);
    }
    then {
      int x = b.age;
      x = x + 1;
      borrower = borrower as Borrower;
    }
  }
}
flowtask body ($p) {
  {
    call task: body > sub;
    $p.foo();
  }
}
functiontask `body>sub` {
  { }
}
"""

  "bound names in a rule" should {

    val cpg = code(arl)

    "REF binding identifiers to the binding local" in {
      val refs = cpg.local.name("b").referencingIdentifiers.l
      refs.size should be >= 2
    }

    "REF local-var identifiers to the local" in {
      // `int x = b.age` (lhs), `x` on the rhs of `x + 1`, `x =` lhs
      cpg.local.name("x").referencingIdentifiers.size shouldBe 3
    }

    "REF every `this` identifier to a METHOD_PARAMETER_IN" in {
      // bare signature-member `borrower` lowers to a `this.borrower` fieldAccess
      val thisIdents = cpg.method.name("bound.rule").ast.isIdentifier.name("this").l
      thisIdents should not be empty
      thisIdents.count(_.refsTo.collectAll[MethodParameterIn].nonEmpty) shouldBe thisIdents.size
    }

    "REF every bound identifier to a declaration" in {
      val bound = cpg.method.name("bound.rule").ast.isIdentifier.name("b", "x", "this").l
      bound should not be empty
      bound.filterNot(_.refsTo.nonEmpty).l shouldBe Nil
    }
  }

  "a flowtask $-param" should {

    val cpg = code(arl)

    "be REF'd by its $-identifiers" in {
      val param = cpg.method.name("body").parameter.name("\\$p").l
      param should not be empty
      param.head.referencingIdentifiers.size should be >= 1
      val bound = cpg.method.name("body").ast.isIdentifier.name("\\$p").l
      bound should not be empty
      bound.filterNot(_.refsTo.nonEmpty).l shouldBe Nil
    }
  }
}
