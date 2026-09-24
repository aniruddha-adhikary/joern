package io.joern.arl2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.joern.x2cpg.X2Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class XomLinkerTests extends AnyWordSpec with Matchers {

  private val arlCode = """import loan.Address;
import loan.Borrower;
import loan.LoanUtil;
public signature S extends java.lang.Object {
  public in Borrower borrower = null;
}
ruleset R (S){
  rule `computation.bankruptcyScore` {
    when {
      Borrower() from borrower;
    }
    then {
      x = borrower.getBankruptcyAge();
      y = borrower.creditScore;
      z = LoanUtil.compute(x);
      w = borrower.address.getZip();
      v = borrower.address.primary;
      Address a = borrower.getAddress();
      q = a.getZip();
    }
  }
}
"""

  private val borrowerJava = """package loan;
public class Borrower {
  public int creditScore;
  public Address address;
  public int getBankruptcyAge() { return 0; }
  public Address getAddress() { return address; }
}
"""

  private val addressJava = """package loan;
public class Address {
  public String getZip() { return ""; }
  public boolean isPrimary() { return true; }
}
"""

  private val loanUtilJava = """package loan;
public class LoanUtil {
  public static int compute(int x) { return x; }
}
"""

  private def withXomCpg(f: Cpg => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("arl2cpg-xom-test") { dir =>
      Files.writeString(dir.resolve("rules.arl"), arlCode)
      val xomDir = dir.resolve("xom")
      Files.createDirectories(xomDir.resolve("loan"))
      Files.writeString(xomDir.resolve("loan/Borrower.java"), borrowerJava)
      Files.writeString(xomDir.resolve("loan/LoanUtil.java"), loanUtilJava)
      Files.writeString(xomDir.resolve("loan/Address.java"), addressJava)

      val config = Config().withInputPath(dir.toString).withXomSrcPaths(Set(xomDir.toString))
      val cpg    = new Arl2Cpg().createCpg(config).get
      try f(cpg)
      finally cpg.close()
    }
  }

  "ARL linked against the XOM" should {

    "type the signature-param fieldAccess with the Java type" in withXomCpg { cpg =>
      val fa = cpg.call.name(Operators.fieldAccess).code(".*borrower.*").l
      fa.exists(_.typeFullName == "loan.Borrower") shouldBe true
    }

    "resolve borrower.getBankruptcyAge() to the Java method" in withXomCpg { cpg =>
      val call = cpg.call.name("getBankruptcyAge").headOption
      call should not be empty
      call.get.methodFullName shouldBe "loan.Borrower.getBankruptcyAge:int()"
    }

    "type borrower.creditScore field access via the Java member" in withXomCpg { cpg =>
      val fa = cpg.call.name(Operators.fieldAccess).code(".*creditScore").l
      fa.exists(_.typeFullName == "int") shouldBe true
    }

    "resolve static LoanUtil.compute" in withXomCpg { cpg =>
      val call = cpg.call.name("compute").headOption
      call should not be empty
      call.get.methodFullName should include("loan.LoanUtil.compute")
      call.get.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "resolve a call through a field-access receiver: borrower.address.getZip()" in withXomCpg { cpg =>
      val calls = cpg.call.name("getZip").l
      calls.size shouldBe 2
      calls.foreach(_.methodFullName shouldBe "loan.Address.getZip:java.lang.String()")
    }

    "type a getter-only Java property: borrower.address.primary" in withXomCpg { cpg =>
      val fa = cpg.call.name(Operators.fieldAccess).code(".*address.primary").l
      fa.size shouldBe 1
      fa.head.typeFullName shouldBe "boolean"
    }

    "propagate assignment types: Address a = borrower.getAddress(); a.getZip()" in withXomCpg { cpg =>
      cpg.local.name("a").head.typeFullName shouldBe "loan.Address"
      val call = cpg.call.name("getZip").l
      call.size shouldBe 2
      call.foreach(_.methodFullName shouldBe "loan.Address.getZip:java.lang.String()")
    }

    "create CALL edges into the Java methods after default overlays" in withXomCpg { cpg =>
      X2Cpg.applyDefaultOverlays(cpg)
      val rule    = cpg.method.name("computation.bankruptcyScore").head
      val callees = rule.call.callee(NoResolve).name.toSet
      callees should contain("getBankruptcyAge")
    }
  }
}
