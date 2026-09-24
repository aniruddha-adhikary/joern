package io.joern.arl2cpg.testfixtures

import io.joern.arl2cpg.{Arl2Cpg, Config}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.validation.{PostFrontendValidator, ValidationLevel}

import java.io.File
import java.nio.file.Paths

trait ArlFrontend extends LanguageFrontend {
  override type ConfigType = Config

  override def execute(sourceCodePath: File): Cpg = {
    val config = getConfig().getOrElse(Config()).withInputPath(sourceCodePath.getAbsolutePath)
    val cpg    = new Arl2Cpg().createCpg(config).get
    PostFrontendValidator(cpg, ValidationLevel.V3).run()
    cpg
  }
}

class ArlDefaultTestCpg extends DefaultTestCpg with ArlFrontend {
  override val fileSuffix: String = ".arl"
}

class Arl2CpgSuite(withPostProcessing: Boolean = false)
    extends Code2CpgFixture(() => new ArlDefaultTestCpg().withPostProcessingPasses(withPostProcessing)) {

  override def code(code: String): ArlDefaultTestCpg = {
    super.code(code, (Paths.get("test.arl")).toString)
  }
}
