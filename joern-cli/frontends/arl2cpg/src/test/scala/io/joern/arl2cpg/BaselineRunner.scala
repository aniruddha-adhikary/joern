package io.joern.arl2cpg

import io.joern.arl2cpg.parser.{ARLParser, ArlParserFacade}
import io.shiftleft.codepropertygraph.generated.nodes.Unknown
import io.shiftleft.semanticcpg.language.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ParseTree

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Completeness baseline: builds every corpus directory with `--allow-unknown`, dumps each UNKNOWN node as TSV and
  * reports which parser rules the corpus never exercises (the alternatives whose lowering is therefore unmeasured).
  *
  * Usage: `sbt "arl2cpg/Test/runMain io.joern.arl2cpg.BaselineRunner <corpusRoot> <unknowns.tsv>"`
  */
object BaselineRunner {

  def main(args: Array[String]): Unit = {
    val root = Paths.get(args(0))
    val out  = new PrintWriter(args(1))
    out.println("corpus\tfile\tline\tparserTypeName\tcode")
    val dirs         = Files.list(root).iterator().asScala.filter(Files.isDirectory(_)).toList.sortBy(_.toString)
    val ruleHits     = mutable.Map.empty[String, Int].withDefaultValue(0)
    var syntaxErrors = 0

    dirs.foreach { dir =>
      Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".arl")).foreach { file =>
        val result = ArlParserFacade.parse(file.toString).get
        syntaxErrors += result.errorCount
        visit(result.compilationUnit, ruleHits)
      }

      val config   = Config().withInputPath(dir.toString).withAllowUnknown(true)
      val cpg      = new Arl2Cpg().createCpg(config).get
      val files    = cpg.file.name.filter(_.endsWith(".arl")).size
      val unknowns = cpg.all.collect { case u: Unknown => u }.l
      System.err.println(s"${dir.getFileName}: $files files, ${unknowns.size} UNKNOWN")
      unknowns.foreach { u =>
        val code = u.code.linesIterator.nextOption().getOrElse("").take(120).replace('\t', ' ')
        out.println(
          s"${dir.getFileName}\t${u.file.name.headOption.getOrElse("")}\t${u.lineNumber.getOrElse(-1)}\t" +
            s"${u.parserTypeName}\t$code"
        )
      }
      System.err.println(s"  findings: " + cpg.finding.l.groupBy(ArlFindings.code).view.mapValues(_.size).toMap)
      cpg.close()
    }
    out.close()

    val allRules = ARLParser.ruleNames.toList
    val unhit    = allRules.filterNot(ruleHits.contains)
    System.err.println(s"syntax errors across corpus: $syntaxErrors")
    System.err.println(s"parser rules: ${allRules.size}, exercised: ${allRules.size - unhit.size}, never exercised:")
    unhit.sorted.foreach(r => System.err.println(s"  $r"))
  }

  private def visit(tree: ParseTree, hits: mutable.Map[String, Int]): Unit = tree match {
    case ctx: ParserRuleContext =>
      hits(ARLParser.ruleNames(ctx.getRuleIndex)) += 1
      (0 until ctx.getChildCount).foreach(i => visit(ctx.getChild(i), hits))
    case _ =>
  }
}
