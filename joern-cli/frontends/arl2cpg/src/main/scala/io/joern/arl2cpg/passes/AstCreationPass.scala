package io.joern.arl2cpg.passes

import io.joern.arl2cpg.Config
import io.joern.arl2cpg.astcreation.AstCreator
import io.joern.arl2cpg.parser.ArlParserFacade
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.joern.x2cpg.frontendspecific.arl2cpg.FileExtensions
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, config: Config)(implicit withSchemaValidation: ValidationMode)
    extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  private val report = new Report()

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
          (false, relPath)
        case Success(parseResult) =>
          val fileLOC = IOUtils.readLinesInFile(Paths.get(filename)).size
          report.addReportInfo(relPath, fileLOC, parsed = true)
          if (parseResult.errorCount > 0) {
            logger.warn(s"'$relPath' produced ${parseResult.errorCount} syntax error(s); continuing with partial AST")
          }
          Try {
            diffGraph.absorb(new AstCreator(parseResult, config).createAst())
          } match {
            case Failure(exception) =>
              logger.warn(s"Failed to generate CPG for '$filename'", exception)
              (false, relPath)
            case Success(_) =>
              (true, relPath)
          }
      }
    }
    report.updateReport(reportName, cpg = gotCpg, duration)
  }
}
