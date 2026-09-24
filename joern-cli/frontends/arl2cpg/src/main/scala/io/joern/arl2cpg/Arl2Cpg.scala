package io.joern.arl2cpg

import io.joern.arl2cpg.passes.{AstCreationPass, XomLinkerPass}
import io.joern.javasrc2cpg.{Config as JavaSrcConfig}
import io.joern.javasrc2cpg.passes.{AstCreationPass as JavaSrcAstCreationPass, OuterClassRefPass, TypeInferencePass}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.frontendspecific.arl2cpg.{FileExtensions, Language}
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.util.Try

class Arl2Cpg extends X2CpgFrontend {
  private val logger = LoggerFactory.getLogger(getClass)

  override type ConfigType = Config
  override val defaultConfig: Config = Config()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      MetaDataPass(cpg, Language, config.inputPath).createAndApply()
      new AstCreationPass(cpg, config)(config.schemaValidation).createAndApply()
      config.xomSrcPaths.foreach(runJavasrcPasses(cpg, _))
      TypeNodePass.withTypesFromCpg(cpg).createAndApply()
      if (config.xomSrcPaths.nonEmpty) {
        new XomLinkerPass(cpg).createAndApply()
      }
    }
  }

  /** Runs javasrc2cpg's passes for the XOM sources into the same CPG, mirroring `JavaSrc2Cpg.createCpg` minus the
    * MetaDataPass (metadata stays `ODMARL`).
    */
  private def runJavasrcPasses(cpg: Cpg, xomSrcDir: String): Unit = {
    Try {
      val javaConfig      = JavaSrcConfig().withInputPath(xomSrcDir)
      val astCreationPass = new JavaSrcAstCreationPass(javaConfig, cpg)
      astCreationPass.createAndApply()
      astCreationPass.sourceParser.cleanupDelombokOutput()
      astCreationPass.clearJavaParserCaches()
      astCreationPass.closeTypeSolvers()
      new OuterClassRefPass(cpg).createAndApply()
      TypeNodePass.withRegisteredTypes(astCreationPass.usedTypes(), cpg).createAndApply()
      new TypeInferencePass(cpg).createAndApply()
    } match {
      case scala.util.Failure(exception) =>
        logger.warn(s"Failed to import XOM sources from '$xomSrcDir'", exception)
      case scala.util.Success(_) =>
        logger.debug(s"Imported XOM sources from '$xomSrcDir'")
    }
  }
}
