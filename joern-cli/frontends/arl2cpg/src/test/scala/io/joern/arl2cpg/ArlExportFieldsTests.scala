package io.joern.arl2cpg

import io.joern.arl2cpg.ArlFindings.{Codes, Keys}
import io.joern.arl2cpg.testfixtures.Arl2CpgSuite
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlExport
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Files, Paths}

class ArlExportFieldsTests extends Arl2CpgSuite() {

  private val source = """import loan.Borrower;
public signature S extends java.lang.Object {
  public in Borrower borrower = null;
}
ruleset R (S) {
  rule `r.export` {
    then {
      Borrower localBorrower = this.borrower;
      copiedBorrower = localBorrower;
      name = this.borrower.lastName;
      matches = localBorrower.lastName.equalsIgnoreCase("CNL");
      stringValue = "CNL";
      intValue = 1;
      longValue = 1L;
      doubleValue = 1.5;
      booleanValue = true;
      charValue = 'c';
      nullValue = null;
      unresolved = this.borrower.noSuchExportMethod();
    }
  }
}
"""

  private def withLoanCpg(f: Cpg => Unit): Unit =
    FileUtil.usingTemporaryDirectory("arl2cpg-export-fields") { dir =>
      val inputDir = dir.resolve("input")
      Files.createDirectories(inputDir)
      Files.writeString(inputDir.resolve("loan-rules.arl"), source)
      val xomSourceDir = Paths.get(getClass.getResource("/xom").toURI).toString
      val cpg          = new Arl2Cpg()
        .createCpg(
          Config()
            .withInputPath(inputDir.toString)
            .withXomSrcPaths(Set(xomSourceDir))
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

  private def assertNumberOrNull(value: ujson.Value): Unit =
    if (value != ujson.Null) {
      value.num
      ()
    }

  "ARL JSON export fields" should {
    "export loan XOM, findings, references, literal types, and JDK callees" in {
      withLoanCpg { cpg =>
        val json = ujson.read(ArlExport.toJson(cpg, "loan-rules.cpg"))
        json.obj.keys.toList.shouldBe(List("cpgFile", "methods", "types", "findings"))
        json("cpgFile").str.shouldBe("loan-rules.cpg")

        val methods    = json("methods").arr
        val methodKeys =
          List("id", "stableId", "name", "fullName", "signature", "filename", "line", "lineEnd", "arlKind", "nodes")
        methods.foreach { method =>
          method.obj.keys.toList.shouldBe(methodKeys)
          method("id").num
          assertNumberOrNull(method("line"))
          assertNumberOrNull(method("lineEnd"))
          method("arlKind").str
        }
        val methodSortKeys = methods.map(method => (method("fullName").str, method("id").num.toLong)).toList
        methodSortKeys.shouldBe(methodSortKeys.sorted)
        methods.nonEmpty.shouldBe(true)

        val ruleMethod = methods.find(method => method("name").str == "r.export").get
        val nodes      = ruleMethod("nodes").arr
        nodes.map(_("id").num.toLong).toList.shouldBe(nodes.map(_("id").num.toLong).sorted)
        val byId = nodes.map(node => node("id").num.toLong -> node).toMap

        nodes.foreach { node =>
          node("id").num
          node("label").str
          node("order").num
          node("code").str
          assertNumberOrNull(node("line"))
          assertNumberOrNull(node("columnNumber"))
          node("children").arr.foreach(_.num)
          node.obj.get("cfgOut").foreach(_.arr.foreach(_.num))
        }

        val local = nodes.find(node => node("label").str == "LOCAL" && node("name").str == "localBorrower").get
        local.obj.keys.toList.takeRight(2).shouldBe(List("name", "typeFullName"))
        local("typeFullName").str.shouldBe("loan.Borrower")
        val localId        = local("id").num.toLong
        val localReference = nodes
          .find(node =>
            node("label").str == "IDENTIFIER" &&
              node("code").str == "localBorrower" &&
              node("refs").arr.exists(_.num.toLong == localId)
          )
          .get
        localReference("name").str.shouldBe("localBorrower")
        localReference("typeFullName").str.shouldBe("loan.Borrower")
        localReference("refs").arr
          .map(_.num.toLong)
          .toList
          .shouldBe(localReference("refs").arr.map(_.num.toLong).sorted.toList)
        localReference("refs").arr.map(_.num.toLong).toSet.should(contain(localId))
        val identifierKeys = localReference.obj.keys.toList
        identifierKeys.takeRight(3).shouldBe(List("name", "typeFullName", "refs"))

        val parameter = nodes.find(_("label").str == "METHOD_PARAMETER_IN").get
        parameter("name").str
        parameter("typeFullName").str
        parameter("index").num

        val fieldIdentifiers = nodes.filter(_("label").str == "FIELD_IDENTIFIER")
        fieldIdentifiers.nonEmpty.shouldBe(true)
        fieldIdentifiers.foreach(_.obj.keys.toList.last.shouldBe("canonicalName"))
        fieldIdentifiers.exists(_("canonicalName").str == "lastName").shouldBe(true)

        val expectedLiteralTypes = List(
          "\"CNL\"" -> "java.lang.String",
          "1"       -> "int",
          "1L"      -> "long",
          "1.5"     -> "double",
          "true"    -> "boolean",
          "'c'"     -> "char",
          "null"    -> "ANY"
        )
        val literals = nodes.filter(_("label").str == "LITERAL")
        literals.foreach(_.obj.keys.toList.last.shouldBe("typeFullName"))
        expectedLiteralTypes.foreach { case (code, typeFullName) =>
          val matching = literals.filter(_("code").str == code)
          matching.nonEmpty.shouldBe(true)
          matching.foreach(_("typeFullName").str.shouldBe(typeFullName))
        }

        val calls = nodes.filter(_("label").str == "CALL")
        calls.foreach { call =>
          call("name").str
          call("methodFullName").str
          call("signature").str
          call("typeFullName").str
          val arguments = call("arguments").arr
          arguments.foreach { argument =>
            argument.obj.keys.toList.shouldBe(List("id", "index"))
            argument("id").num
            argument("index").num
          }
          val argumentSortKeys = arguments.map(argument => (argument("index").num.toInt, argument("id").num.toLong))
          argumentSortKeys.toList.shouldBe(argumentSortKeys.sorted.toList)
          val callees = call("callees").arr
          callees.foreach { callee =>
            callee.obj.keys.toList.shouldBe(List("id", "fullName", "external"))
            callee("id").num
            callee("fullName").str
            callee("external").bool
          }
          val calleeSortKeys = callees.map(callee => (callee("fullName").str, callee("id").num.toLong))
          calleeSortKeys.toList.shouldBe(calleeSortKeys.sorted.toList)
        }

        val jdkCall     = calls.find(_("name").str == "equalsIgnoreCase").get
        val jdkFullName = "java.lang.String.equalsIgnoreCase:boolean(java.lang.String)"
        jdkCall("methodFullName").str.shouldBe(jdkFullName)
        val arguments = jdkCall("arguments").arr
        arguments.map(_("index").num.toInt).toList.shouldBe(List(0, 1))
        byId(arguments.head("id").num.toLong)("code").str.endsWith("localBorrower.lastName").shouldBe(true)
        val jdkCallee        = jdkCall("callees").arr.find(_("fullName").str == jdkFullName)
        val actualJdkCallees = ujson.write(jdkCall("callees"))
        withClue(s"Expected external JDK callee $jdkFullName; actual=$actualJdkCallees: ") {
          jdkCallee should not be empty
        }
        val resolvedJdkCallee = jdkCallee.get
        resolvedJdkCallee("id").num
        resolvedJdkCallee("external").bool.shouldBe(true)

        val types = json("types").arr
        types.foreach { typeDecl =>
          typeDecl.obj.keys.toList.shouldBe(List("id", "stableId", "name", "fullName", "file", "inherits", "members"))
          typeDecl("id").num
          typeDecl("file").str
          typeDecl("inherits").arr.map(_.str).toList.shouldBe(typeDecl("inherits").arr.map(_.str).sorted.toList)
          val members = typeDecl("members").arr
          members.foreach { member =>
            member.obj.keys.toList.shouldBe(List("id", "stableId", "name", "typeFullName", "code", "line"))
            member("id").num
            member("typeFullName").str
            member("code").str
            assertNumberOrNull(member("line"))
          }
          val memberSortKeys = members.map(member => (member("name").str, member("id").num.toLong)).toList
          memberSortKeys.shouldBe(memberSortKeys.sorted)
        }
        val typeSortKeys = types.map(typeDecl => (typeDecl("fullName").str, typeDecl("id").num.toLong)).toList
        typeSortKeys.shouldBe(typeSortKeys.sorted)
        val loanTypes = types.filter(_("fullName").str.startsWith("loan."))
        loanTypes
          .map(_("fullName").str)
          .toSet
          .should(
            contain allOf (
              "loan.Address",
              "loan.Borrower",
              "loan.Loan",
              "loan.LoanRequest",
              "loan.LoanUtil",
              "loan.Report",
              "loan.Ssn"
            )
          )
        val borrowerType = loanTypes.find(_("fullName").str == "loan.Borrower").get
        borrowerType("inherits").arr.nonEmpty.shouldBe(true)
        borrowerType("file").str.endsWith("Borrower.java").shouldBe(true)
        borrowerType("members").arr.map(_("name").str).toSet.should(contain("lastName"))
        loanTypes.exists(_("inherits").arr.exists(_.str == "loan.LoanRequest")).shouldBe(true)

        val findings = json("findings").arr
        findings.nonEmpty.shouldBe(true)
        findings.exists(finding => finding.obj.contains("code") && finding.obj.contains("reason")).shouldBe(true)
        findings.foreach { finding =>
          finding.obj.foreach { case (key, value) =>
            if (key == Keys.Candidates) value.arr.foreach(_.str)
            else value.str
          }
        }
        val unresolvedCall = cpg.call.nameExact("noSuchExportMethod").head
        val unresolvedTarget = findings
          .find(_.obj.get(Keys.CallId).contains(ujson.Str(unresolvedCall.id.toString)))
          .get
        unresolvedTarget(Keys.Code).str.shouldBe(Codes.UnresolvedCallTarget)
        unresolvedTarget(Keys.Severity).str.shouldBe("unresolved")
        unresolvedTarget(Keys.Candidates).arr.shouldBe(empty)
        val stableIdsByNodeId = json("methods").arr
          .flatMap(_("nodes").arr)
          .map(node => node("id").num.toLong -> node("stableId").str)
          .toMap
        val expectedFindings = cpg.finding.l.sortBy(_.id).map { finding =>
          val pairs = finding.keyValuePairs.map(pair => pair.key -> pair.value).toList
          val withCallStableId = pairs.find(_._1 == "callId") match {
            case None              => pairs
            case Some((_, callId)) => pairs :+ ("callStableId" -> stableIdsByNodeId(callId.toLong))
          }
          val valuesByKey = withCallStableId.groupMap(_._1)(_._2)
          val findingCode = valuesByKey.get(Keys.Code).flatMap(_.headOption).getOrElse("")
        val requiredListKeys =
          if (findingCode == Codes.UnresolvedCallTarget) Set(Keys.Candidates) else Set.empty[String]
          (valuesByKey.keySet ++ requiredListKeys).toList.sorted.map { key =>
            val values = valuesByKey.getOrElse(key, Nil)
            val value =
              if (ArlFindings.ListValuedKeys.contains(key)) ujson.Arr.from(values.sorted.map(ujson.Str(_)))
              else ujson.Str(values.head)
            key -> value
          }
        }
        val actualFindings = findings.map { finding =>
          finding.obj.toList.sortBy(_._1)
        }.toList
        actualFindings.shouldBe(expectedFindings)
      }
    }
  }
}
