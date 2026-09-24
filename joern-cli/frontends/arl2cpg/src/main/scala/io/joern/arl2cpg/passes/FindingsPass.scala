package io.joern.arl2cpg.passes

import io.joern.arl2cpg.ArlFindings
import io.joern.arl2cpg.ArlFindings.Codes
import io.shiftleft.codepropertygraph.generated.nodes.Unknown
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Gate 1 (arlgraph DESIGN.md §4.2): every parse-tree node must be consumed by the lowering.
  *
  * Records one FINDING per UNKNOWN node the AST creators fell back to and one per file with syntax errors; a mapping
  * element the B2X reader does not model (recorded by [[B2xEffectsPass]]) counts the same way. After the findings are
  * in the graph, [[Gate1Violation]] is thrown unless the build was started with `--allow-unknown`: the CPG is still
  * written (the caller closes it), but the exit status says it is not a complete lowering.
  */
class FindingsPass(cpg: Cpg, diagnostics: ParseDiagnostics, allowUnknown: Boolean) extends CpgPass(cpg) {

  private var unknownCount     = 0
  private var syntaxErrorFiles = 0
  private var b2xUnmodelled    = 0

  override def run(builder: DiffGraphBuilder): Unit = {
    val unknowns = cpg.all.collect { case u: Unknown => u }.filter(_.file.name.exists(_.endsWith(".arl"))).l
    unknowns.foreach { unknown =>
      val filename = unknown.file.name.headOption.getOrElse("")
      ArlFindings.finding(
        builder,
        Option(unknown),
        Codes.UnknownConstruct,
        unknown.parserTypeName,
        s"no lowering for ${unknown.parserTypeName}: ${unknown.code.linesIterator.nextOption().getOrElse("")}",
        filename,
        unknown.lineNumber
      )
    }
    unknownCount = unknowns.size

    val syntaxErrors = diagnostics.syntaxErrors
    syntaxErrors.foreach { case (filename, count) =>
      val fileNode = cpg.file.nameExact(filename).headOption
      ArlFindings.finding(
        builder,
        fileNode,
        Codes.SyntaxError,
        Codes.SyntaxError,
        s"$count syntax error(s); the AST under this file is error recovery, not the program",
        filename,
        None
      )
    }
    syntaxErrorFiles = syntaxErrors.size
    b2xUnmodelled = cpg.finding.count(f => ArlFindings.code(f) == Codes.B2xUnmodelledElement)
  }

  override def finish(): Unit = {
    if (!allowUnknown && (unknownCount > 0 || syntaxErrorFiles > 0 || b2xUnmodelled > 0)) {
      throw Gate1Violation(unknownCount, syntaxErrorFiles, b2xUnmodelled)
    }
  }
}

final case class Gate1Violation(unknownConstructs: Int, filesWithSyntaxErrors: Int, unmodelledB2xElements: Int)
    extends RuntimeException(
      s"incomplete lowering: $unknownConstructs unknown construct(s), $filesWithSyntaxErrors file(s) with syntax " +
        s"errors, $unmodelledB2xElements unmodelled b2x element(s) — see FINDING nodes (code=unknown-construct / " +
        "syntax-error / b2x-unmodelled-element); pass --allow-unknown to accept a tainted CPG"
    )
