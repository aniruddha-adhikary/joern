package io.joern.arl2cpg

import io.joern.arl2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters for the arl2cpg frontend.
  */
final case class Config(
  xomSrcPaths: Set[String] = Set.empty,
  rflSrcPaths: Set[String] = Set.empty,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config] {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config =
    copy(genericConfig = value)

  def withXomSrcPaths(paths: Set[String]): Config = copy(xomSrcPaths = paths)

  def withRflSrcPaths(paths: Set[String]): Config = copy(rflSrcPaths = paths)
}

private object Frontend {
  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("arl2cpg"),
      opt[String]("xom-src")
        .unbounded()
        .action((xomPath, config) => config.copy(xomSrcPaths = config.xomSrcPaths + xomPath))
        .text(
          "path to Java sources of the eXecution Object Model (XOM). Repeatable. The Java sources are " +
            "imported into the same CPG and ARL calls are linked against them."
        ),
      opt[String]("rfl-src")
        .unbounded()
        .action((rflPath, config) => config.copy(rflSrcPaths = config.rflSrcPaths + rflPath))
        .text(
          "path to ODM ruleflow metadata (.rfl files, searched recursively). Repeatable. The metadata " +
            "disambiguates flow tasks that share a visible name by scoping their fullName with the " +
            "ruleflow uuid."
        )
    )
  }
}

object Main extends X2CpgMain(new Arl2Cpg(), cmdLineParser)
