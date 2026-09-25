package io.joern.arl2cpg.passes

import io.joern.arl2cpg.Config
import io.joern.arl2cpg.astcreation.AstCreator
import io.joern.arl2cpg.identity.{TaskIdentity, TaskIdentityFile}
import io.joern.arl2cpg.parser.ArlParserFacade
import io.joern.arl2cpg.rfl.RflMetadata
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.joern.x2cpg.frontendspecific.arl2cpg.FileExtensions
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

/** Syntax-error counts per file, collected while the AST is built so [[FindingsPass]] can report them. */
class ParseDiagnostics {
  private val errors = new java.util.concurrent.ConcurrentHashMap[String, Int]()

  def record(filename: String, errorCount: Int): Unit = if (errorCount > 0) errors.put(filename, errorCount)

  def syntaxErrors: Map[String, Int] = {
    import scala.jdk.CollectionConverters.*
    errors.asScala.toMap
  }
}

class AstCreationPass(cpg: Cpg, config: Config, diagnostics: ParseDiagnostics = new ParseDiagnostics)(implicit
  withSchemaValidation: ValidationMode
) extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  private val report = new Report()

  private val rflMetadata       = RflMetadata.load(config.rflSrcPaths.toSeq)
  private val taskIdentityIndex = TaskIdentity.load(config.taskIdentityPaths.toSeq)

  private val sourceFiles =
    SourceFiles.determine(
      config.inputPath,
      FileExtensions,
      ignoredDefaultRegex = Option(config.defaultIgnoredFilesRegex),
      ignoredFilesRegex = Option(config.ignoredFilesRegex),
      ignoredFilesPath = Option(config.ignoredFiles)
    )

  override def generateParts(): Array[String] = sourceFiles.toArray

  override def finish(): Unit = {
    report.print()
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val relPath                          = SourceFiles.toRelativePath(filename, config.inputPath)
    val ((gotCpg, reportName), duration) = TimeUtils.time {
      ArlParserFacade.parse(filename) match {
        case Failure(exception) =>
          logger.warn(s"Failed to parse '$filename'", exception)
          diagnostics.record(filename, 1)
          (false, relPath)
        case Success(parseResult) =>
          val fileLOC = IOUtils.readLinesInFile(Paths.get(filename)).size
          report.addReportInfo(relPath, fileLOC, parsed = true)
          if (parseResult.errorCount > 0) {
            logger.warn(s"'$relPath' produced ${parseResult.errorCount} syntax error(s); continuing with partial AST")
            diagnostics.record(filename, parseResult.errorCount)
          }
          Try {
            val taskIdentity =
              if (taskIdentityIndex.isEmpty) TaskIdentityFile.empty
              else {
                val bytes = Files.readAllBytes(Paths.get(filename))
                taskIdentityIndex.forFile(Paths.get(filename).getFileName.toString, TaskIdentity.sha256Hex(bytes))
              }
            diffGraph.absorb(new AstCreator(parseResult, config, rflMetadata, taskIdentity).createAst())
          } match {
            case Failure(exception) =>
              logger.warn(s"Failed to generate CPG for '$filename'", exception)
              diagnostics.record(filename, 1)
              (false, relPath)
            case Success(_) =>
              (true, relPath)
          }
      }
    }
    report.updateReport(reportName, cpg = gotCpg, duration)
  }
}
