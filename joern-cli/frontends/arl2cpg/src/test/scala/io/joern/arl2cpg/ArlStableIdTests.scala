package io.joern.arl2cpg

import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlExport
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.jar.{JarEntry, JarOutputStream}
import javax.tools.ToolProvider
import scala.jdk.CollectionConverters.*
import scala.io.Source
import scala.util.Using

class ArlStableIdTests extends AnyWordSpec with Matchers {

  private val xomSourceDir = Paths.get(getClass.getResource("/xom").toURI).toString
  private val b2xPath      = Paths.get(getClass.getResource("/b2x/loan.b2x").toURI).toString

  private val loanSource = """import loan.Borrower;
import loan.ClasspathProbe;
public signature S extends java.lang.Object {
  public in Borrower borrower = null;
}
ruleset R (S) {
  rule `r.first` {
    then {
      normalized = ClasspathProbe.normalize(this.borrower.lastName);
    }
  }
  rule `r.second` {
    then {
      copiedName = this.borrower.lastName;
    }
  }
}
"""

  private val bundledNames =
    Seq("branch", "chain", "forkjoin", "loop", "modes", "prio", "select", "subflow", "upd", "xcheck")

  private lazy val branchHeader =
    Using.resource(Source.fromResource("arl/branch.arl"))(_.getLines().take(19).mkString("\n"))
  private lazy val branchSource =
    Using.resource(Source.fromResource("arl/branch.arl"))(_.mkString)

  private def unknownCallsSource(ruleCount: Int): String = {
    val rules = (0 until ruleCount).map { index =>
      s"""rule `probe.r$index` {
         |  when {
         |    Borrower() from borrower;
         |    evaluate ( borrower.noSuchMethod$index() );
         |  }
         |  then {
         |    score = Integer.valueOf(1);
         |  }
         |}
         |""".stripMargin
    }
    s"$branchHeader\n${rules.mkString("\n")}\n}\n"
  }

  private def withClasspathJar(root: Path): Path = {
    val source = root.resolve("classpath-src/loan/ClasspathProbe.java")
    Files.createDirectories(source.getParent)
    Files.writeString(
      source,
      """package loan;
        |public final class ClasspathProbe {
        |  public static String normalize(String value) { return value; }
        |}
        |""".stripMargin
    )
    val classes = root.resolve("classpath-classes")
    Files.createDirectories(classes)
    val compiler = Option(ToolProvider.getSystemJavaCompiler).getOrElse {
      throw new IllegalStateException("The test requires a JDK Java compiler")
    }
    compiler.run(null, null, null, "-d", classes.toString, source.toString) shouldBe 0

    val jar  = root.resolve("classpath-probe.jar")
    val out  = new JarOutputStream(Files.newOutputStream(jar))
    val walk = Files.walk(classes)
    try {
      walk.iterator().asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString).foreach { classFile =>
        val entryName = classes.relativize(classFile).toString.replace('\\', '/')
        out.putNextEntry(new JarEntry(entryName))
        Files.copy(classFile, out)
        out.closeEntry()
      }
    } finally {
      walk.close()
      out.close()
    }
    jar
  }

  private def createCpg(inputDir: Path, classpath: Seq[String] = Nil, withB2x: Boolean = false): Cpg = {
    var config = Config()
      .withInputPath(inputDir.toString)
      .withXomSrcPaths(Set(xomSourceDir))
      .withXomClasspath(classpath)
      .withAllowUnknown(true)
    if (withB2x) config = config.withB2xPath(b2xPath)
    new Arl2Cpg().createCpg(config).get
  }

  private def exportJson(inputDir: Path, classpath: Seq[String] = Nil, withB2x: Boolean = false): ujson.Value = {
    val cpg = createCpg(inputDir, classpath, withB2x)
    try {
      X2Cpg.applyDefaultOverlays(cpg)
      ujson.read(ArlExport.toJson(cpg, "stable-ids.cpg"))
    } finally cpg.close()
  }

  private def exportedArlObjects(root: ujson.Value): List[(Long, String)] =
    root("methods").arr
      .filter(_("filename").str.endsWith(".arl"))
      .flatMap { method =>
        val methodObject = List(method("id").num.toLong -> method("stableId").str)
        val nodes        = method("nodes").arr.map(node => node("id").num.toLong -> node("stableId").str).toList
        methodObject ++ nodes
      }
      .toList
      .sortBy(_._1)

  private def stableIdsInRule(root: ujson.Value, name: String): List[String] = {
    val method = root("methods").arr.find(_("name").str == name).get
    (List(method("stableId").str) ++ method("nodes").arr.map(_("stableId").str)).sorted
  }

  private def stableFilename(cpg: Cpg, filename: String): String = {
    val path = Paths.get(filename).normalize()
    val stablePath =
      if (path.isAbsolute) {
        Paths.get(cpg.metaData.root.head).toAbsolutePath.normalize().relativize(path.toAbsolutePath.normalize())
      } else path
    stablePath.toString.replace('\\', '/')
  }

  private def methodStableKey(cpg: Cpg, method: Method): String =
    s"METHOD:${method.fullName}@${stableFilename(cpg, method.filename)}"

  private def stableKeyForNode(cpg: Cpg, node: AstNode): String = {
    var current: AstNode    = node
    var path                = List.empty[AstNode]
    var enclosingMethod     = Option.empty[Method]
    while (enclosingMethod.isEmpty) {
      current match {
        case method: Method => enclosingMethod = Some(method)
        case astNode =>
          val parent = astNode.astParent
          if (parent == null) {
            throw new IllegalStateException(s"Node ${node.id} has no enclosing METHOD")
          }
          path = astNode :: path
          current = parent
      }
    }
    methodStableKey(cpg, enclosingMethod.get) + path.map(child => s"/${child.label}:${child.order}").mkString
  }

  private def stableIdForKey(key: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(key.getBytes(StandardCharsets.UTF_8))
      .take(16)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  "ARL stable IDs" should {
    "preserve ARL node ids and stableIds across XOM and B2X configurations" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-stable-id-configs") { dir =>
        val inputDir = dir.resolve("input")
        Files.createDirectories(inputDir)
        Files.writeString(inputDir.resolve("loan-rules.arl"), loanSource)
        val classpathJar = withClasspathJar(dir)

        val base     = exportedArlObjects(exportJson(inputDir))
        val withJar  = exportedArlObjects(exportJson(inputDir, Seq(classpathJar.toString)))
        val withB2x  = exportedArlObjects(exportJson(inputDir, withB2x = true))
        val withBoth = exportedArlObjects(exportJson(inputDir, Seq(classpathJar.toString), withB2x = true))
        withJar.shouldBe(base)
        withB2x.shouldBe(base)
        withBoth.shouldBe(base)

        base.foreach { case (_, stableId) => stableId.matches("[0-9a-f]{32}").shouldBe(true) }
        val cpg = createCpg(inputDir)
        try {
          X2Cpg.applyDefaultOverlays(cpg)
          val firstRule = ujson.read(ArlExport.toJson(cpg, "stable-ids.cpg"))
          val method    = firstRule("methods").arr.find(_("name").str == "r.first").get
          val graphMethod = cpg.method.l.find(_.id == method("id").num.toLong).get
          Paths.get(method("filename").str).isAbsolute.shouldBe(true)
          stableFilename(cpg, method("filename").str).shouldBe("loan-rules.arl")
          method("stableId").str.shouldBe(stableIdForKey(methodStableKey(cpg, graphMethod)))

          val normalizeCall =
            method("nodes").arr.find(node => node("label").str == "CALL" && node("name").str == "normalize").get
          val graphCall = cpg.call.l.find(_.id == normalizeCall("id").num.toLong).get
          normalizeCall("stableId").str.shouldBe(stableIdForKey(stableKeyForNode(cpg, graphCall)))

          val borrowerType = firstRule("types").arr.find(_("fullName").str == "loan.Borrower").get
          val graphType    = cpg.typeDecl.l.find(_.id == borrowerType("id").num.toLong).get
          val typeKey      = s"TYPE_DECL:${graphType.fullName}@${stableFilename(cpg, graphType.filename)}"
          borrowerType("stableId").str.shouldBe(stableIdForKey(typeKey))
          val borrowerMember = borrowerType("members").arr.head
          borrowerMember("stableId").str.shouldBe(stableIdForKey(s"$typeKey/MEMBER:${borrowerMember("name").str}"))
          val callFinding = firstRule("findings").arr.find(_.obj.contains("callId")).get
          val callId      = callFinding("callId").str.toLong
          val callNode    = firstRule("methods").arr
            .flatMap(_("nodes").arr)
            .find(_("id").num.toLong == callId)
            .get
          callFinding("callStableId").str.shouldBe(callNode("stableId").str)
        } finally cpg.close()
      }
    }

    "preserve stable keys for non-twin methods" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-stable-id-non-twin") { dir =>
        val inputDir = dir.resolve("input")
        Files.createDirectories(inputDir)
        Files.writeString(inputDir.resolve("loan-rules.arl"), loanSource)
        val root   = exportJson(inputDir)
        val method = root("methods").arr.find(_("name").str == "r.first").get
        val key    = s"METHOD:${method("fullName").str}@loan-rules.arl"
        method("stableKey").str.shouldBe(key)
        method("stableKey").str.contains("#").shouldBe(false)
        method("stableId").str.shouldBe(stableIdForKey(key))
      }
    }

    "export duplicate computationFlow methods with source-ordered stable keys in every configuration" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-stable-id-twins") { dir =>
        val inputDir = dir.resolve("input")
        Files.createDirectories(inputDir)
        val duplicate =
          """flowtask computationFlow($EngineData){
            |  {
            |    call task : computationFlow>computation;
            |  }
            |}
            |""".stripMargin
        Files.writeString(inputDir.resolve("branch.arl"), s"$branchSource\n$duplicate")
        val classpathJar = withClasspathJar(dir)

        def exportedTwins(root: ujson.Value): List[(String, String, List[String])] =
          root("methods").arr
            .filter(_("name").str == "computationFlow")
            .sortBy(_("stableKey").str)
            .map { method =>
              (
                method("stableKey").str,
                method("stableId").str,
                method("nodes").arr.map(_("stableId").str).toList.sorted
              )
            }
            .toList

        val base     = exportJson(inputDir)
        val withJar  = exportJson(inputDir, Seq(classpathJar.toString))
        val withB2x  = exportJson(inputDir, withB2x = true)
        val withBoth = exportJson(inputDir, Seq(classpathJar.toString), withB2x = true)

        val twins = base("methods").arr
          .filter(_("name").str == "computationFlow")
          .sortBy(_("line").num)
          .toList
        twins.size.shouldBe(2)
        twins.map(_("stableKey").str.takeRight(2)).shouldBe(List("#0", "#1"))
        twins.map(_("stableId").str).distinct.size.shouldBe(2)
        twins.foreach { method =>
          val nodeStableIds = method("nodes").arr.map(_("stableId").str).toList
          nodeStableIds.distinct.shouldBe(nodeStableIds)
          method("nodes").arr.foreach(_.obj.contains("stableKey").shouldBe(false))
        }
        val allNodeStableIds = twins.flatMap(method => method("nodes").arr.map(_("stableId").str))
        allNodeStableIds.distinct.size.shouldBe(allNodeStableIds.size)

        exportedTwins(withJar).shouldBe(exportedTwins(base))
        exportedTwins(withB2x).shouldBe(exportedTwins(base))
        exportedTwins(withBoth).shouldBe(exportedTwins(base))
      }
    }

    "keep other rules stable when a statement is inserted into one rule" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-stable-id-edit") { dir =>
        val inputDir = dir.resolve("input")
        Files.createDirectories(inputDir)
        val inputFile = inputDir.resolve("loan-rules.arl")
        Files.writeString(inputFile, loanSource)
        val before = stableIdsInRule(exportJson(inputDir), "r.second")

        val edited = loanSource.replace(
          "      normalized = ClasspathProbe.normalize(this.borrower.lastName);\n",
          "      extra = true;\n      normalized = ClasspathProbe.normalize(this.borrower.lastName);\n"
        )
        Files.writeString(inputFile, edited)
        val after = stableIdsInRule(exportJson(inputDir), "r.second")
        after.shouldBe(before)
      }
    }

    "export 4,000 unresolved-call findings in under 30 seconds" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-export-scale") { dir =>
        val inputDir = dir.resolve("input")
        Files.createDirectories(inputDir)
        Files.writeString(inputDir.resolve("scale.arl"), unknownCallsSource(4000))
        val cpg = createCpg(inputDir)
        try {
          val started = System.nanoTime()
          val root    = ujson.read(ArlExport.toJson(cpg, "scale.cpg"))
          val seconds = (System.nanoTime() - started).toDouble / 1_000_000_000d
          info(f"4,000-rule ARL export completed in $seconds%.3f seconds")
          root("findings").arr.size should be >= 4000
          seconds should be < 30.0
        } finally cpg.close()
      }
    }

    "export unique stableIds for the bundled corpus" in {
      FileUtil.usingTemporaryDirectory("arl2cpg-stable-id-corpus") { dir =>
        bundledNames.foreach { name =>
          val source = Using.resource(Source.fromResource(s"arl/$name.arl"))(_.mkString)
          Files.writeString(dir.resolve(s"$name.arl"), source)
        }
        val root = exportJson(dir)
        root("methods").arr.foreach { method =>
          val nodeIds = method("nodes").arr.map(_("id").num.toLong).toList
          nodeIds.distinct.shouldBe(nodeIds)
        }
        val entries = root("methods").arr.flatMap { method =>
          method("nodes").arr.map(node => node("id").num.toLong -> node("stableId").str) :+
            (method("id").num.toLong -> method("stableId").str)
        }.toList ++ root("types").arr.flatMap { typeDecl =>
          typeDecl("members").arr.map(member => member("id").num.toLong -> member("stableId").str) :+
            (typeDecl("id").num.toLong -> typeDecl("stableId").str)
        }.toList
        val entriesByNodeId = entries.groupBy(_._1)
        entriesByNodeId.values.foreach { nodeEntries =>
          nodeEntries.map(_._2).distinct.size.shouldBe(1)
        }
        val stableIds = entriesByNodeId.values.map(_.head._2).toList
        stableIds.distinct.size.shouldBe(stableIds.size)
        stableIds.foreach(_.matches("[0-9a-f]{32}").shouldBe(true))
      }
    }
  }
}
