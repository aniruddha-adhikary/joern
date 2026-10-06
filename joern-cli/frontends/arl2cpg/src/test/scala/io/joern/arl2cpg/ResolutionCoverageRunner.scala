package io.joern.arl2cpg

import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}

object ResolutionCoverageRunner {

  private val UnresolvedCallTarget = "unresolved-call-target"
  private val Header               = "filename\tline\tcode\tmethodFullName"

  final case class ResolvedCallRow(filename: String, line: Int, code: String, methodFullName: String) {
    def tsv: String = s"$filename\t$line\t$code\t$methodFullName"
  }

  def main(args: Array[String]): Unit = {
    require(args.length >= 2, "Usage: <arlDir> <xomSrcDir> [--xom-classpath p]... [--resolved-out tsv]")
    val arlDir    = Paths.get(args(0)).toAbsolutePath.normalize()
    val xomSrcDir = Paths.get(args(1)).toString
    var output    = Option.empty[Path]
    var classpath = Vector.empty[String]
    var index     = 2
    while (index < args.length) {
      args(index) match {
        case "--xom-classpath" if index + 1 < args.length =>
          classpath :+= args(index + 1)
          index += 2
        case "--resolved-out" if index + 1 < args.length =>
          output = Some(Paths.get(args(index + 1)))
          index += 2
        case other =>
          throw new IllegalArgumentException(s"Unexpected or incomplete argument: $other")
      }
    }

    val config = Config()
      .withInputPath(arlDir.toString)
      .withXomSrcPaths(Set(xomSrcDir))
      .withXomClasspath(classpath)
      .withAllowUnknown(true)
    val cpg = new Arl2Cpg().createCpg(config).get
    try {
      val calls      = arlCalls(cpg)
      val resolved   = calls.filter(isResolved)
      val unresolved = calls.size - resolved.size
      println(s"total non-operator CALLs: ${calls.size}")
      println(s"resolved: ${resolved.size}")
      println(s"unresolved: $unresolved")

      val findingsByReason = cpg.finding
        .filter(finding => ArlFindings.code(finding) == UnresolvedCallTarget)
        .l
        .groupBy(ArlFindings.reason)
        .toList
        .sortBy(_._1)
      println(
        if (findingsByReason.isEmpty) "unresolved by reason: none"
        else
          findingsByReason
            .map { case (reason, findings) => s"$reason=${findings.size}" }
            .mkString("unresolved by reason: ", ", ", "")
      )

      output.foreach { path =>
        Option(path.getParent).foreach(Files.createDirectories(_))
        val writer = new PrintWriter(path.toFile)
        try {
          writer.println(Header)
          resolvedRows(resolved, arlDir).foreach(row => writer.println(row.tsv))
        } finally writer.close()
      }
    } finally cpg.close()
  }

  def resolvedCallRows(cpg: Cpg, arlDir: Path): List[ResolvedCallRow] =
    resolvedRows(arlCalls(cpg).filter(isResolved), arlDir.toAbsolutePath.normalize())

  private def arlCalls(cpg: Cpg): List[Call] =
    cpg.call
      .filter(call => call.file.name.exists(_.endsWith(".arl")))
      .filterNot(_.name.startsWith("<operator>."))
      .l

  private def isResolved(call: Call): Boolean =
    !call.methodFullName.contains(Defines.UnresolvedSignature) &&
      !call.methodFullName.contains(Defines.UnresolvedNamespace)

  private def resolvedRows(calls: List[Call], root: Path): List[ResolvedCallRow] =
    calls
      .map { call =>
        val sourceName = call.file.name.headOption.getOrElse("")
        val sourcePath = Paths.get(sourceName)
        val absolute   = if (sourcePath.isAbsolute) sourcePath.normalize() else root.resolve(sourcePath).normalize()
        val filename   =
          (if (absolute.startsWith(root)) root.relativize(absolute).toString else sourceName).replace('\\', '/')
        ResolvedCallRow(
          filename,
          call.lineNumber.getOrElse(-1),
          call.code.replace('\t', ' ').replace('\n', ' '),
          call.methodFullName
        )
      }
      .sortBy(row => (row.filename, row.line, row.code, row.methodFullName))
}
