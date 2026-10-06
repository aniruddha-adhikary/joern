package io.joern.arl2cpg

import io.joern.arl2cpg.ArlFindings.{Codes, Keys}
import io.joern.x2cpg.Defines
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlExport
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}
import javax.tools.ToolProvider
import scala.jdk.CollectionConverters.*

class OverloadResolutionTests extends AnyWordSpec with Matchers {

  private def ruleSource(body: String, imports: Seq[String] = Nil, signatureFields: String = ""): String = {
    val allImports = (Seq("import loan.Borrower;") ++ imports).distinct
    s"""${allImports.mkString("\n")}
public signature S extends java.lang.Object {
  $signatureFields
}
ruleset R (S) {
  rule `resolution.test` {
    then {
      $body
    }
  }
}
"""
  }

  private def withCpg(
    body: String,
    imports: Seq[String] = Nil,
    signatureFields: String = "",
    javaSources: Map[String, String] = Map.empty,
    classpath: Seq[String] = Seq.empty,
    allowUnknown: Boolean = true
  )(test: Cpg => Unit): Unit =
    FileUtil.usingTemporaryDirectory("arl2cpg-overload-test") { dir =>
      withCpgAt(dir, body, imports, signatureFields, javaSources, classpath, allowUnknown)(test)
    }

  private def withCpgAt(
    dir: Path,
    body: String,
    imports: Seq[String],
    signatureFields: String,
    javaSources: Map[String, String],
    classpath: Seq[String],
    allowUnknown: Boolean
  )(test: Cpg => Unit): Unit = {
    Files.writeString(dir.resolve("rules.arl"), ruleSource(body, imports, signatureFields))
    val xomDir = dir.resolve("xom")
    javaSources.toList.sortBy(_._1).foreach { case (relativePath, code) =>
      val file = xomDir.resolve(relativePath)
      Files.createDirectories(file.getParent)
      Files.writeString(file, code)
    }
    var config = Config().withInputPath(dir.toString).withAllowUnknown(allowUnknown)
    if (javaSources.nonEmpty) config = config.withXomSrcPaths(Set(xomDir.toString))
    if (classpath.nonEmpty) config = config.withXomClasspath(classpath)
    val cpg = new Arl2Cpg().createCpg(config).get
    try test(cpg)
    finally cpg.close()
  }

  private def findCall(cpg: Cpg, name: String, code: String) =
    cpg.call.name(name).filter(_.code == code).headOption

  private def expectCall(
    cpg: Cpg,
    name: String,
    code: String,
    methodFullName: String,
    typeFullName: String,
    dispatchType: String = DispatchTypes.STATIC_DISPATCH
  ) = {
    val call    = findCall(cpg, name, code)
    val details = call.map { value =>
      val args = value.argument.l.sortBy(_.argumentIndex).map { arg =>
        val argType = arg match {
          case call: Call             => call.typeFullName
          case identifier: Identifier => identifier.typeFullName
          case literal: Literal       => literal.typeFullName
          case _                      => "<no-type>"
        }
        s"${arg.argumentIndex}:${arg.code}:$argType"
      }
      val finding = findingFor(cpg, value.id()).map { unresolved =>
        s"${ArlFindings.reason(unresolved)}:${ArlFindings.values(unresolved, Keys.Candidates)}"
      }
      s"method=${value.methodFullName}, type=${value.typeFullName}, args=$args, finding=$finding"
    }
    withClue(s"Expected call '$code'; actual=$details. ") { call should not be empty }
    withClue(s"Expected target $methodFullName; actual=$details. ") {
      call.get.methodFullName shouldBe methodFullName
    }
    call.get.typeFullName shouldBe typeFullName
    call.get.dispatchType shouldBe dispatchType
    call.get
  }

  private def findingFor(cpg: Cpg, callId: Long) =
    ArlFindings
      .findings(cpg, Codes.UnresolvedCallTarget)
      .find(finding => ArlFindings.value(finding, Keys.CallId) == callId.toString)

  private def compileStubSources(root: Path): (Path, Path) = {
    val sources = Map(
      "loan/CpUtil.java" ->
        """package loan;
          |public class CpUtil {
          |  public static int objectCount(Object[] values) { return values.length; }
          |  public static int stringCount(String... values) { return values.length; }
          |  public static ilog.rules.brl.Date echoDate(ilog.rules.brl.Date value) { return value; }
          |  public static String echoString(String value) { return value; }
          |  static int packagePrivate(int value) { return value; }
          |}
          |""".stripMargin,
      "ilog/rules/brl/Date.java" ->
        """package ilog.rules.brl;
          |public class Date extends java.util.Date {
          |  public String name;
          |  public String getCode() { return name; }
          |  public Date() { super(); }
          |}
          |""".stripMargin,
      "ilog/rules/brl/IlrCollectionUtil.java" ->
        """package ilog.rules.brl;
          |public class IlrCollectionUtil {
          |  public static boolean isIn(java.util.Collection values, Object value) { return false; }
          |  public static int getSize(java.util.Collection values) { return values.size(); }
          |  public static int getSize(Object[] values) { return values.length; }
          |}
          |""".stripMargin,
      "ilog/rules/brl/SimpleDate.java" ->
        """package ilog.rules.brl;
          |public class SimpleDate {
          |  public SimpleDate(long value) { }
          |}
          |""".stripMargin,
      "ilog/rules/brl/Engine.java" ->
        """package ilog.rules.brl;
          |public class Engine {
          |  public void note(String value) { }
          |}
          |""".stripMargin,
      "pkg/Outer.java" ->
        """package pkg;
          |public class Outer {
          |  public static class Inner {
          |    public static int nested(int value) { return value; }
          |  }
          |}
          |""".stripMargin
    )
    val sourceDir  = root.resolve("stub-sources")
    val classesDir = root.resolve("stub-classes")
    Files.createDirectories(classesDir)
    val files = sources.toList.sortBy(_._1).map { case (relativePath, code) =>
      val file = sourceDir.resolve(relativePath)
      Files.createDirectories(file.getParent)
      Files.writeString(file, code)
      file.toFile
    }
    val compiler = ToolProvider.getSystemJavaCompiler
    compiler should not be null
    val manager = compiler.getStandardFileManager(null, null, null)
    try {
      val units = manager.getJavaFileObjectsFromFiles(files.asJava)
      val task  = compiler.getTask(null, manager, null, List("-d", classesDir.toString).asJava, null, units)
      task.call() shouldBe java.lang.Boolean.TRUE
    } finally manager.close()

    val jar    = root.resolve("stubs.jar")
    val output = new JarOutputStream(Files.newOutputStream(jar))
    val stream = Files.walk(classesDir)
    try {
      stream
        .iterator()
        .asScala
        .filter(file => Files.isRegularFile(file) && file.toString.endsWith(".class"))
        .toList
        .sortBy(_.toString)
        .foreach { file =>
          val entryName = classesDir.relativize(file).toString.replace('\\', '/')
          output.putNextEntry(new JarEntry(entryName))
          Files.copy(file, output)
          output.closeEntry()
        }
    } finally {
      stream.close()
      output.close()
    }
    jar -> classesDir
  }

  private val borrowerJava =
    """package loan;
      |public class Borrower {
      |  public Borrower() {}
      |  public String name;
      |}
      |""".stripMargin

  "argument-type-aware Java call resolution" should {

    "choose primitive overloads by exact type and widening" in withCpg(
      """Util.m(1);
        |Util.m(1L);
        |Wide.onlyLong(1);
        |""".stripMargin,
      imports = Seq("import loan.Util;", "import loan.Wide;"),
      javaSources = Map(
        "loan/Util.java" ->
          """package loan;
            |public class Util {
            |  public static int m(int value) { return value; }
            |  public static long m(long value) { return value; }
            |}
            |""".stripMargin,
        "loan/Wide.java" ->
          """package loan;
            |public class Wide {
            |  public static long onlyLong(long value) { return value; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(cpg, "m", "Util.m(1)", "loan.Util.m:int(int)", "int")
      expectCall(cpg, "m", "Util.m(1L)", "loan.Util.m:long(long)", "long")
      expectCall(cpg, "onlyLong", "Wide.onlyLong(1)", "loan.Wide.onlyLong:long(long)", "long")
    }

    "apply boxing and unboxing only after strict invocation conversions" in withCpg(
      """BoxUtil.box(1);
        |BothUtil.choose(1);
        |UnboxUtil.unbox(number);
        |""".stripMargin,
      imports =
        Seq("import loan.BoxUtil;", "import loan.BothUtil;", "import loan.UnboxUtil;", "import java.lang.Integer;"),
      signatureFields = "public in Integer number = null;",
      javaSources = Map(
        "loan/BoxUtil.java" ->
          """package loan;
            |public class BoxUtil {
            |  public static Integer box(Integer value) { return value; }
            |}
            |""".stripMargin,
        "loan/BothUtil.java" ->
          """package loan;
            |public class BothUtil {
            |  public static int choose(int value) { return value; }
            |  public static Integer choose(Integer value) { return value; }
            |}
            |""".stripMargin,
        "loan/UnboxUtil.java" ->
          """package loan;
            |public class UnboxUtil {
            |  public static int unbox(int value) { return value; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(
        cpg,
        "box",
        "BoxUtil.box(1)",
        "loan.BoxUtil.box:java.lang.Integer(java.lang.Integer)",
        "java.lang.Integer"
      )
      expectCall(cpg, "choose", "BothUtil.choose(1)", "loan.BothUtil.choose:int(int)", "int")
      expectCall(cpg, "unbox", "UnboxUtil.unbox(number)", "loan.UnboxUtil.unbox:int(int)", "int")
    }

    "use fixed arity before varargs and support empty and expanded varargs calls" in withCpg(
      """Util.m();
        |Util.m("a");
        |Util.m("a", "b", "c");
        |Util.pick("a");
        |""".stripMargin,
      imports = Seq("import loan.Util;"),
      javaSources = Map(
        "loan/Util.java" ->
          """package loan;
            |public class Util {
            |  public static int m(String... values) { return values.length; }
            |  public static int pick(String value) { return 1; }
            |  public static int pick(String... values) { return values.length; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(cpg, "m", "Util.m()", "loan.Util.m:int(java.lang.String[])", "int")
      expectCall(cpg, "m", "Util.m(\"a\")", "loan.Util.m:int(java.lang.String[])", "int")
      expectCall(cpg, "m", "Util.m(\"a\", \"b\", \"c\")", "loan.Util.m:int(java.lang.String[])", "int")
      expectCall(cpg, "pick", "Util.pick(\"a\")", "loan.Util.pick:int(java.lang.String)", "int")
    }

    "select the most-specific reference parameter" in withCpg(
      """Util.choose("text");
        |Util.choose(borrower);
        |Util.choose(null);
        |""".stripMargin,
      imports = Seq("import loan.Util;"),
      signatureFields = "public in Borrower borrower = null;",
      javaSources = Map(
        "loan/Borrower.java" -> borrowerJava,
        "loan/Util.java"     ->
          """package loan;
            |public class Util {
            |  public static String choose(Object value) { return ""; }
            |  public static String choose(String value) { return value; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(
        cpg,
        "choose",
        "Util.choose(\"text\")",
        "loan.Util.choose:java.lang.String(java.lang.String)",
        "java.lang.String"
      )
      expectCall(
        cpg,
        "choose",
        "Util.choose(borrower)",
        "loan.Util.choose:java.lang.String(java.lang.Object)",
        "java.lang.String"
      )
      expectCall(
        cpg,
        "choose",
        "Util.choose(null)",
        "loan.Util.choose:java.lang.String(java.lang.String)",
        "java.lang.String"
      )
    }

    "inherit methods, retain overriding declarations, and expose Object methods on XOM types" in withCpg(
      """child.inherited();
        |child.onlyInterface();
        |check.onlyInterface();
        |child.overridden();
        |child.equals(other);
        |check.equals(other);
        |""".stripMargin,
      imports = Seq("import loan.Child;", "import loan.Checkable;"),
      signatureFields = "public in Child child = null; public in Child other = null; public in Checkable check = null;",
      javaSources = Map(
        "loan/Parent.java" ->
          """package loan;
            |public class Parent {
            |  public int inherited() { return 1; }
            |  public int overridden() { return 1; }
            |}
            |""".stripMargin,
        "loan/Checkable.java" ->
          """package loan;
            |public interface Checkable { int onlyInterface(); }
            |""".stripMargin,
        "loan/Child.java" ->
          """package loan;
            |public abstract class Child extends Parent implements Checkable {
            |  public int overridden() { return 2; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(
        cpg,
        "inherited",
        "child.inherited()",
        "loan.Parent.inherited:int()",
        "int",
        DispatchTypes.DYNAMIC_DISPATCH
      )
      expectCall(
        cpg,
        "onlyInterface",
        "child.onlyInterface()",
        "loan.Checkable.onlyInterface:int()",
        "int",
        DispatchTypes.DYNAMIC_DISPATCH
      )
      expectCall(
        cpg,
        "onlyInterface",
        "check.onlyInterface()",
        "loan.Checkable.onlyInterface:int()",
        "int",
        DispatchTypes.DYNAMIC_DISPATCH
      )
      expectCall(
        cpg,
        "overridden",
        "child.overridden()",
        "loan.Child.overridden:int()",
        "int",
        DispatchTypes.DYNAMIC_DISPATCH
      )
      expectCall(
        cpg,
        "equals",
        "child.equals(other)",
        "java.lang.Object.equals:boolean(java.lang.Object)",
        "boolean",
        DispatchTypes.DYNAMIC_DISPATCH
      )
      expectCall(
        cpg,
        "equals",
        "check.equals(other)",
        "java.lang.Object.equals:boolean(java.lang.Object)",
        "boolean",
        DispatchTypes.DYNAMIC_DISPATCH
      )
    }

    "filter instance methods from static-owner calls" in withCpg(
      "Util.m(1);",
      imports = Seq("import loan.Util;"),
      javaSources = Map(
        "loan/Parent.java" ->
          """package loan;
            |public class Parent { public int m(int value) { return value; } }
            |""".stripMargin,
        "loan/Util.java" ->
          """package loan;
            |public class Util extends Parent { public static int m(int value) { return value; } }
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(cpg, "m", "Util.m(1)", "loan.Util.m:int(int)", "int")
    }

    "retain the constructed type for constructor calls" in withCpg(
      "new Borrower();",
      javaSources = Map("loan/Borrower.java" -> borrowerJava)
    ) { cpg =>
      val constructor = cpg.call.name("<init>").head
      constructor.methodFullName shouldBe "loan.Borrower.<init>:void()"
      constructor.typeFullName shouldBe "loan.Borrower"
      constructor.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }

    "not inherit constructors from a superclass" in withCpg(
      """new Sub("x");""",
      imports = Seq("import loan.Sub;"),
      javaSources = Map(
        "loan/Base.java" ->
          """package loan;
            |public class Base {
            |  public Base(String value) {}
            |}
            |""".stripMargin,
        "loan/Sub.java" ->
          """package loan;
            |public class Sub extends Base {
            |  public Sub(int value) { super("x"); }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      val constructor = cpg.call.name("<init>").filter(_.file.name.exists(_.endsWith(".arl"))).head
      constructor.methodFullName should include("<unresolvedSignature>")
      val finding = findingFor(cpg, constructor.id()).get
      ArlFindings.reason(finding) shouldBe "no-candidate"
      ArlFindings.values(finding, Keys.Candidates) shouldBe List("loan.Sub.<init>:void(int)")
    }

    "consider only public source methods" in withCpg(
      """Util.m(1);
        |PackageUtil.only(1);
        |""".stripMargin,
      imports = Seq("import loan.Util;", "import loan.PackageUtil;"),
      javaSources = Map(
        "loan/Util.java" ->
          """package loan;
            |public class Util {
            |  private static int m(int value) { return value; }
            |  public static long m(long value) { return value; }
            |}
            |""".stripMargin,
        "loan/PackageUtil.java" ->
          """package loan;
            |public class PackageUtil {
            |  static int only(int value) { return value; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(cpg, "m", "Util.m(1)", "loan.Util.m:long(long)", "long")
      val inaccessible = findCall(cpg, "only", "PackageUtil.only(1)").get
      ArlFindings.reason(findingFor(cpg, inaccessible.id()).get) shouldBe "no-candidate"
    }

    "not infer a member type from a private getter" in withCpg(
      "GetterUtil.choose(hidden.code);",
      imports = Seq("import loan.HiddenGetter;", "import loan.GetterUtil;"),
      signatureFields = "public in HiddenGetter hidden = null;",
      javaSources = Map(
        "loan/HiddenGetter.java" ->
          """package loan;
            |public class HiddenGetter {
            |  private String getCode() { return ""; }
            |}
            |""".stripMargin,
        "loan/GetterUtil.java" ->
          """package loan;
            |public class GetterUtil {
            |  public static int choose(String value) { return 1; }
            |  public static int choose(Object value) { return 2; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      val call = findCall(cpg, "choose", "GetterUtil.choose(hidden.code)").get
      ArlFindings.reason(findingFor(cpg, call.id()).get) shouldBe "ambiguous"
    }

    "leave incomparable overloads unresolved with sorted ambiguity evidence" in withCpg(
      "Util.choose(first, second);",
      imports = Seq("import loan.Util;", "import java.lang.Integer;"),
      signatureFields = "public in Integer first = null; public in Integer second = null;",
      javaSources = Map(
        "loan/Util.java" ->
          """package loan;
            |public class Util {
            |  public static int choose(Integer first, Object second) { return 1; }
            |  public static int choose(Object first, Integer second) { return 2; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      val call = findCall(cpg, "choose", "Util.choose(first, second)").get
      call.methodFullName should include("<unresolvedSignature>")
      val finding = findingFor(cpg, call.id()).get
      ArlFindings.reason(finding) shouldBe "ambiguous"
      val candidates = List(
        "loan.Util.choose:int(java.lang.Integer,java.lang.Object)",
        "loan.Util.choose:int(java.lang.Object,java.lang.Integer)"
      ).sorted
      ArlFindings.values(finding, Keys.Candidates) shouldBe candidates
      val exportedFinding = ujson
        .read(ArlExport.toJson(cpg, "overloads.cpg"))("findings")
        .arr
        .find(_.obj.get(Keys.CallId).contains(ujson.Str(call.id.toString)))
        .get
      exportedFinding(Keys.Candidates).arr.map(_.str).toList shouldBe candidates
    }

    "report no-candidate and unknown-receiver findings without tripping Gate 1" in withCpg(
      """borrower.missing(1);
        |unknownReceiver.missing(1);
        |""".stripMargin,
      signatureFields = "public in Borrower borrower = null;",
      javaSources = Map("loan/Borrower.java" -> borrowerJava),
      allowUnknown = false
    ) { cpg =>
      val noCandidate = findCall(cpg, "missing", "borrower.missing(1)").get
      val unknown     = findCall(cpg, "missing", "unknownReceiver.missing(1)").get
      ArlFindings.reason(findingFor(cpg, noCandidate.id()).get) shouldBe "no-candidate"
      ArlFindings.value(findingFor(cpg, noCandidate.id()).get, Keys.ReceiverType) shouldBe "loan.Borrower"
      ArlFindings.reason(findingFor(cpg, unknown.id()).get) shouldBe "receiver-unknown"
      ArlFindings.value(findingFor(cpg, unknown.id()).get, Keys.ReceiverType) shouldBe "ANY"
    }

    "propagate member and resolved-call return types bottom-up" in withCpg(
      "Boolean.valueOf(LoanUtil.isFilled(borrower.name)).booleanValue();",
      imports = Seq("import loan.LoanUtil;", "import java.lang.Boolean;"),
      signatureFields = "public in Borrower borrower = null;",
      javaSources = Map(
        "loan/Borrower.java" -> borrowerJava,
        "loan/LoanUtil.java" ->
          """package loan;
            |public class LoanUtil {
            |  public static boolean isFilled(String value) { return value != null; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(
        cpg,
        "isFilled",
        "LoanUtil.isFilled(borrower.name)",
        "loan.LoanUtil.isFilled:boolean(java.lang.String)",
        "boolean"
      )
      expectCall(
        cpg,
        "valueOf",
        "Boolean.valueOf(LoanUtil.isFilled(borrower.name))",
        "java.lang.Boolean.valueOf:java.lang.Boolean(boolean)",
        "java.lang.Boolean"
      )
      expectCall(
        cpg,
        "booleanValue",
        "Boolean.valueOf(LoanUtil.isFilled(borrower.name)).booleanValue()",
        "java.lang.Boolean.booleanValue:boolean()",
        "boolean",
        DispatchTypes.DYNAMIC_DISPATCH
      )
      cpg.call.name(Operators.fieldAccess).find(_.code.endsWith("borrower.name")).get.typeFullName shouldBe
        "java.lang.String"
    }

    "resolve JDK overloads and return types from jrt" in withCpg(
      """String.valueOf(number);
        |Integer.valueOf(0);
        |text.equals(other);
        |text.length();
        |""".stripMargin,
      imports = Seq("import java.lang.String;", "import java.lang.Integer;"),
      signatureFields = "public in int number = 0; public in String text = null; public in String other = null;",
      javaSources = Map("loan/Unused.java" -> "package loan; public class Unused {}")
    ) { cpg =>
      expectCall(
        cpg,
        "valueOf",
        "String.valueOf(number)",
        "java.lang.String.valueOf:java.lang.String(int)",
        "java.lang.String"
      )
      expectCall(
        cpg,
        "valueOf",
        "Integer.valueOf(0)",
        "java.lang.Integer.valueOf:java.lang.Integer(int)",
        "java.lang.Integer"
      )
      expectCall(
        cpg,
        "equals",
        "text.equals(other)",
        "java.lang.String.equals:boolean(java.lang.Object)",
        "boolean",
        DispatchTypes.DYNAMIC_DISPATCH
      )
      expectCall(cpg, "length", "text.length()", "java.lang.String.length:int()", "int", DispatchTypes.DYNAMIC_DISPATCH)
    }

    "load classpath jars and class directories lazily and preserve ordered CLI arguments" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-overload-classpath") { dir =>
        val (jar, classesDir) = compileStubSources(dir)
        val body              =
          """CpUtil.objectCount(new Object[]{"a"});
            |CpUtil.stringCount("a", "b");
            |CpUtil.packagePrivate(1);
            |CpUtil.echoDate(date);
            |CpUtil.echoString(date.name);
            |CpUtil.echoString(date.code);
            |""".stripMargin
        val imports = Seq("import loan.CpUtil;")
        Seq(jar, classesDir).foreach { classpath =>
          withCpgAt(
            dir,
            body,
            imports :+ "import ilog.rules.brl.Date;",
            "public in Date date = null;",
            Map.empty,
            Seq(classpath.toString),
            allowUnknown = true
          ) { cpg =>
            val objectCount = expectCall(
              cpg,
              "objectCount",
              """CpUtil.objectCount(new Object[]{"a"})""",
              "loan.CpUtil.objectCount:int(java.lang.Object[])",
              "int"
            )
            val stringCount = expectCall(
              cpg,
              "stringCount",
              """CpUtil.stringCount("a", "b")""",
              "loan.CpUtil.stringCount:int(java.lang.String[])",
              "int"
            )
            val packagePrivate = findCall(cpg, "packagePrivate", "CpUtil.packagePrivate(1)").get
            ArlFindings.reason(findingFor(cpg, packagePrivate.id()).get) shouldBe "no-candidate"
            expectCall(
              cpg,
              "echoDate",
              "CpUtil.echoDate(date)",
              "loan.CpUtil.echoDate:ilog.rules.brl.Date(ilog.rules.brl.Date)",
              "ilog.rules.brl.Date"
            )
            val fieldType = expectCall(
              cpg,
              "echoString",
              "CpUtil.echoString(date.name)",
              "loan.CpUtil.echoString:java.lang.String(java.lang.String)",
              "java.lang.String"
            )
            val getterType = expectCall(
              cpg,
              "echoString",
              "CpUtil.echoString(date.code)",
              "loan.CpUtil.echoString:java.lang.String(java.lang.String)",
              "java.lang.String"
            )
            X2Cpg.applyDefaultOverlays(cpg)
            objectCount.callee(NoResolve).fullName.l.should(contain("loan.CpUtil.objectCount:int(java.lang.Object[])"))
            stringCount.callee(NoResolve).fullName.l.should(contain("loan.CpUtil.stringCount:int(java.lang.String[])"))
            fieldType
              .callee(NoResolve)
              .fullName
              .l
              .should(contain("loan.CpUtil.echoString:java.lang.String(java.lang.String)"))
            getterType
              .callee(NoResolve)
              .fullName
              .l
              .should(contain("loan.CpUtil.echoString:java.lang.String(java.lang.String)"))
          }
        }
      }
      val parsed = Main.parseConfig(Array("--xom-classpath", "first.jar", "--xom-classpath", "classes"))
      parsed.map(_.xomClasspath) shouldBe Some(Seq("first.jar", "classes"))
    }

    "resolve package-qualified static types, fields, and instance receivers" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-package-qualified-types") { dir =>
        val (jar, _) = compileStubSources(dir)
        val body     =
          """ilog.rules.brl.IlrCollectionUtil.isIn(this.items, "x");
            |IlrCollectionUtil.isIn(this.items, "x");
            |IlrCollectionUtil.getSize(this.items);
            |new ilog.rules.brl.SimpleDate(0L);
            |new java.math.BigDecimal(1);
            |d1.before(d2);
            |java.lang.Boolean.valueOf(this.borrower.lastName.isEmpty());
            |java.lang.System.out.println("x");
            |ilog.rules.brl.Engine.this.note("x");
            |foo.bar.baz(1);
            |""".stripMargin
        val imports = Seq(
          "import java.util.List;",
          "import java.math.BigDecimal;",
          "import ilog.rules.brl.Date;",
          "import ilog.rules.brl.IlrCollectionUtil;"
        )
        val signatureFields =
          "public in List items = null; public in Date d1 = null; public in Date d2 = null; " +
            "public in Borrower borrower = null;"
        val javaSources = Map(
          "loan/Borrower.java" ->
            """package loan;
              |public class Borrower { public String lastName; }
              |""".stripMargin
        )
        val allImports     = (Seq("import loan.Borrower;") ++ imports).distinct.mkString("\n")
        val arlWithPattern =
          s"""$allImports
             |public signature S extends java.lang.Object {
             |  $signatureFields
             |}
             |ruleset R (S) {
             |  rule `resolution.test` {
             |    when {
             |      Borrower(lastName.equalsIgnoreCase("pattern")) from borrower;
             |    }
             |    then {
             |      $body
             |    }
             |  }
             |}
             |""".stripMargin
        Files.writeString(dir.resolve("rules.arl"), arlWithPattern)
        val xomDir = dir.resolve("xom")
        Files.createDirectories(xomDir.resolve("loan"))
        Files.writeString(xomDir.resolve("loan/Borrower.java"), javaSources("loan/Borrower.java"))
        val config = Config()
          .withInputPath(dir.toString)
          .withXomSrcPaths(Set(xomDir.toString))
          .withXomClasspath(Seq(jar.toString))
          .withAllowUnknown(true)
        val cpg = new Arl2Cpg().createCpg(config).get
        try {
          def expectConstructor(codeFragment: String, methodFullName: String, typeFullName: String): Unit = {
            val call = cpg.call.nameExact("<init>").find(_.code.contains(codeFragment)).get
            call.methodFullName shouldBe methodFullName
            call.typeFullName shouldBe typeFullName
            call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
          }

          expectCall(
            cpg,
            "isIn",
            """ilog.rules.brl.IlrCollectionUtil.isIn(this.items, "x")""",
            "ilog.rules.brl.IlrCollectionUtil.isIn:boolean(java.util.Collection,java.lang.Object)",
            "boolean"
          )
          expectCall(
            cpg,
            "isIn",
            """IlrCollectionUtil.isIn(this.items, "x")""",
            "ilog.rules.brl.IlrCollectionUtil.isIn:boolean(java.util.Collection,java.lang.Object)",
            "boolean"
          )
          expectCall(
            cpg,
            "getSize",
            "IlrCollectionUtil.getSize(this.items)",
            "ilog.rules.brl.IlrCollectionUtil.getSize:int(java.util.Collection)",
            "int"
          )
          expectConstructor("SimpleDate", "ilog.rules.brl.SimpleDate.<init>:void(long)", "ilog.rules.brl.SimpleDate")
          expectConstructor("BigDecimal", "java.math.BigDecimal.<init>:void(int)", "java.math.BigDecimal")
          expectCall(
            cpg,
            "before",
            "d1.before(d2)",
            "java.util.Date.before:boolean(java.util.Date)",
            "boolean",
            DispatchTypes.DYNAMIC_DISPATCH
          )
          expectCall(
            cpg,
            "valueOf",
            "java.lang.Boolean.valueOf(this.borrower.lastName.isEmpty())",
            "java.lang.Boolean.valueOf:java.lang.Boolean(boolean)",
            "java.lang.Boolean"
          )
          expectCall(
            cpg,
            "println",
            """java.lang.System.out.println("x")""",
            "java.io.PrintStream.println:void(java.lang.String)",
            "void",
            DispatchTypes.DYNAMIC_DISPATCH
          )
          expectCall(
            cpg,
            "note",
            """ilog.rules.brl.Engine.this.note("x")""",
            "ilog.rules.brl.Engine.note:void(java.lang.String)",
            "void",
            DispatchTypes.DYNAMIC_DISPATCH
          )
          val lowercaseCall = findCall(cpg, "baz", "foo.bar.baz(1)").get
          lowercaseCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
          lowercaseCall.methodFullName shouldBe s"${Defines.UnresolvedNamespace}.baz:${Defines.UnresolvedSignature}(2)"
          ArlFindings.reason(findingFor(cpg, lowercaseCall.id()).get) shouldBe "receiver-unknown"
          cpg.call.nameExact("equalsIgnoreCase").l.map(_.methodFullName).distinct shouldBe
            List("java.lang.String.equalsIgnoreCase:boolean(java.lang.String)")
        } finally cpg.close()
      }
    }

    "resolve dotted nested classpath and JDK types" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-overload-nested-types") { dir =>
        val (jar, _) = compileStubSources(dir)
        withCpgAt(
          dir,
          """Inner.nested(1);
            |Entry.comparingByKey();
            |""".stripMargin,
          Seq("import pkg.Outer.Inner;", "import java.util.Map.Entry;"),
          "",
          Map.empty,
          Seq(jar.toString),
          allowUnknown = true
        ) { cpg =>
          expectCall(cpg, "nested", "Inner.nested(1)", "pkg.Outer$Inner.nested:int(int)", "int")
          expectCall(
            cpg,
            "comparingByKey",
            "Entry.comparingByKey()",
            "java.util.Map$Entry.comparingByKey:java.util.Comparator()",
            "java.util.Comparator"
          )
        }
      }
    }

    "infer conditional expression types without computing unrelated reference lubs" in withCpg(
      """Util.numeric(flag ? 1 : 2L);
        |Util.reference(flag ? null : "s");
        |Util.boxed(flag ? integer : 1);
        |Util.boxed(flag ? null : 1);
        |Util.small(flag ? smallByte : smallShort);
        |Util.bool(flag ? boxedFlag : flag);
        |Util.supertype(flag ? child : base);
        |Util.unrelated(flag ? left : right);
        |""".stripMargin,
      imports = Seq(
        "import loan.Util;",
        "import loan.Base;",
        "import loan.Child;",
        "import loan.Left;",
        "import loan.Right;",
        "import java.lang.Integer;"
      ),
      signatureFields =
        "public in boolean flag = false; public in Boolean boxedFlag = null; public in Integer integer = null; public in byte smallByte = 0; public in short smallShort = 0; public in Base base = null; public in Child child = null; public in Left left = null; public in Right right = null;",
      javaSources = Map(
        "loan/Base.java" ->
          """package loan;
            |public class Base {}
            |""".stripMargin,
        "loan/Child.java" ->
          """package loan;
            |public class Child extends Base {}
            |""".stripMargin,
        "loan/Left.java" ->
          """package loan;
            |public class Left {}
            |""".stripMargin,
        "loan/Right.java" ->
          """package loan;
            |public class Right {}
            |""".stripMargin,
        "loan/Util.java" ->
          """package loan;
            |public class Util {
            |  public static int numeric(int value) { return value; }
            |  public static long numeric(long value) { return value; }
            |  public static int reference(String value) { return 1; }
            |  public static int reference(Object value) { return 2; }
            |  public static int boxed(int value) { return value; }
            |  public static int boxed(Integer value) { return value; }
            |  public static int small(int value) { return value; }
            |  public static int small(short value) { return value; }
            |  public static int bool(boolean value) { return value ? 1 : 0; }
            |  public static int bool(Boolean value) { return value ? 1 : 0; }
            |  public static int supertype(Base value) { return 1; }
            |  public static int supertype(Child value) { return 2; }
            |  public static int unrelated(Left value) { return 1; }
            |  public static int unrelated(Right value) { return 2; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(cpg, "numeric", "Util.numeric(flag ? 1 : 2L)", "loan.Util.numeric:long(long)", "long")
      expectCall(
        cpg,
        "reference",
        """Util.reference(flag ? null : "s")""",
        "loan.Util.reference:int(java.lang.String)",
        "int"
      )
      expectCall(cpg, "boxed", "Util.boxed(flag ? integer : 1)", "loan.Util.boxed:int(int)", "int")
      expectCall(cpg, "boxed", "Util.boxed(flag ? null : 1)", "loan.Util.boxed:int(java.lang.Integer)", "int")
      expectCall(cpg, "small", "Util.small(flag ? smallByte : smallShort)", "loan.Util.small:int(short)", "int")
      expectCall(cpg, "bool", "Util.bool(flag ? boxedFlag : flag)", "loan.Util.bool:int(boolean)", "int")
      expectCall(cpg, "supertype", "Util.supertype(flag ? child : base)", "loan.Util.supertype:int(loan.Base)", "int")

      val unrelated = findCall(cpg, "unrelated", "Util.unrelated(flag ? left : right)").get
      ArlFindings.reason(findingFor(cpg, unrelated.id()).get) shouldBe "ambiguous"
    }

    "keep import-resolved cast types exact and reject an unrelated Date overload" in withCpg(
      """LoanUtil.acceptJava((Date) borrower);
        |LoanUtil.acceptCustom((Date) borrower);
        |""".stripMargin,
      imports = Seq("import loan.LoanUtil;", "import ilog.rules.brl.Date;"),
      signatureFields = "public in Borrower borrower = null;",
      javaSources = Map(
        "loan/Borrower.java"       -> borrowerJava,
        "ilog/rules/brl/Date.java" ->
          """package ilog.rules.brl;
            |public class Date { }
            |""".stripMargin,
        "loan/LoanUtil.java" ->
          """package loan;
            |public class LoanUtil {
            |  public static int acceptJava(java.util.Date value) { return 1; }
            |  public static int acceptCustom(ilog.rules.brl.Date value) { return 1; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      val javaDate = findCall(cpg, "acceptJava", "LoanUtil.acceptJava((Date) borrower)").get
      javaDate.methodFullName should include("<unresolvedSignature>")
      ArlFindings.reason(findingFor(cpg, javaDate.id()).get) shouldBe "no-candidate"
      expectCall(
        cpg,
        "acceptCustom",
        "LoanUtil.acceptCustom((Date) borrower)",
        "loan.LoanUtil.acceptCustom:int(ilog.rules.brl.Date)",
        "int"
      )
    }

    "type array initializers, allow array covariance, and resolve ARL array contains" in withCpg(
      """LoanUtil.acceptObjects(new String[]{"a", "b"});
        |LoanUtil.acceptObjects(new Object[]{"a"});
        |LoanUtil.acceptVarargs(new String[]{"a"});
        |values.contains("a");
        |""".stripMargin,
      imports = Seq("import loan.LoanUtil;", "import java.lang.String;"),
      signatureFields = "public in String[] values = null;",
      javaSources = Map(
        "loan/LoanUtil.java" ->
          """package loan;
            |public class LoanUtil {
            |  public static int acceptObjects(Object[] values) { return values.length; }
            |  public static int acceptVarargs(String... values) { return values.length; }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(
        cpg,
        "acceptObjects",
        """LoanUtil.acceptObjects(new String[]{"a", "b"})""",
        "loan.LoanUtil.acceptObjects:int(java.lang.Object[])",
        "int"
      )
      expectCall(
        cpg,
        "acceptObjects",
        """LoanUtil.acceptObjects(new Object[]{"a"})""",
        "loan.LoanUtil.acceptObjects:int(java.lang.Object[])",
        "int"
      )
      expectCall(
        cpg,
        "acceptVarargs",
        """LoanUtil.acceptVarargs(new String[]{"a"})""",
        "loan.LoanUtil.acceptVarargs:int(java.lang.String[])",
        "int"
      )
      val arrays = cpg.call.name(Operators.arrayInitializer).map(_.typeFullName).toSet
      arrays should contain("java.lang.String[]")
      arrays should contain("java.lang.Object[]")
      val contains = findCall(cpg, "contains", """values.contains("a")""").get
      contains.methodFullName shouldBe "java.lang.String[].contains:boolean(java.lang.String)"
      contains.signature shouldBe "boolean(java.lang.String)"
      contains.typeFullName shouldBe "boolean"
    }

    "match generic arguments against erased Java descriptors" in withCpg(
      "LoanUtil.count(names);",
      imports = Seq("import loan.LoanUtil;", "import java.util.List;"),
      signatureFields = "public in List<String> names = null;",
      javaSources = Map(
        "loan/LoanUtil.java" ->
          """package loan;
            |import java.util.List;
            |public class LoanUtil {
            |  public static int count(List<?> values) { return values.size(); }
            |}
            |""".stripMargin
      )
    ) { cpg =>
      expectCall(cpg, "count", "LoanUtil.count(names)", "loan.LoanUtil.count:int(java.util.List)", "int")
    }

    "link XOM method calls after default overlays" in withCpg(
      "LoanUtil.ready(1);",
      imports = Seq("import loan.LoanUtil;"),
      javaSources = Map(
        "loan/LoanUtil.java" ->
          """package loan;
            |public class LoanUtil { public static boolean ready(int value) { return true; } }
            |""".stripMargin
      )
    ) { cpg =>
      val call = expectCall(cpg, "ready", "LoanUtil.ready(1)", "loan.LoanUtil.ready:boolean(int)", "boolean")
      X2Cpg.applyDefaultOverlays(cpg)
      call.callee(NoResolve).fullName.l.should(contain("loan.LoanUtil.ready:boolean(int)"))
    }

    "produce identical calls and unresolved findings across repeated builds" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-overload-determinism") { dir =>
        val sources = Map(
          "loan/Util.java" ->
            """package loan;
              |public class Util {
              |  public static int choose(Integer first, Object second) { return 1; }
              |  public static int choose(Object first, Integer second) { return 2; }
              |  public static int resolved(int value) { return value; }
              |}
              |""".stripMargin
        )
        val body = "Util.choose(first, second); Util.resolved(1);"
        def snapshot(): (List[(String, String)], List[(String, List[(String, String)])]) = {
          var result: Option[(List[(String, String)], List[(String, List[(String, String)])])] = None
          withCpgAt(
            dir,
            body,
            Seq("import loan.Util;", "import java.lang.Integer;"),
            "public in Integer first = null; public in Integer second = null;",
            sources,
            Seq.empty,
            allowUnknown = true
          ) { cpg =>
            val calls = cpg.call
              .filter(_.file.name.exists(_.endsWith(".arl")))
              .filterNot(_.name.startsWith("<operator>."))
              .map(call => call.code -> call.methodFullName)
              .l
              .sorted
            val findings = ArlFindings
              .findings(cpg, Codes.UnresolvedCallTarget)
              .map { finding =>
                val evidenceCode = finding.evidence.headOption.collect { case call: Call => call.code }.getOrElse("")
                evidenceCode -> finding.keyValuePairs.map(kv => kv.key -> kv.value).toList.sortBy(_._1)
              }
              .l
              .sortBy(_._1)
            result = Some(calls -> findings)
          }
          result.get
        }
        snapshot() shouldBe snapshot()
      }
    }
  }
}
