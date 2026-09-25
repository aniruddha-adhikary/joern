package io.joern.arl2cpg

import io.joern.arl2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters for the arl2cpg frontend.
  *
  * @param xomSrcPaths
  *   Java sources of the eXecution Object Model, imported into the same CPG.
  * @param taskIdentityPaths
  *   task identity sidecars (JSON lines) tying flattened task declarations to authored ruleflow uuids.
  * @param b2xPath
  *   the archive's BOM-to-XOM mapping (`b2x.b2x`), which holds the bodies of the methods the ARL only calls.
  * @param allowUnknown
  *   when false (the default), any construct lowered to an UNKNOWN node or any syntax error fails the build after the
  *   findings have been written. Mirrors arlgraph's Gate 1.
  */
final case class Config(
  xomSrcPaths: Set[String] = Set.empty,
  rflSrcPaths: Set[String] = Set.empty,
  taskIdentityPaths: Set[String] = Set.empty,
  b2xPath: Option[String] = None,
  allowUnknown: Boolean = false,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config] {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config =
    copy(genericConfig = value)

  def withXomSrcPaths(paths: Set[String]): Config = copy(xomSrcPaths = paths)

  def withRflSrcPaths(paths: Set[String]): Config = copy(rflSrcPaths = paths)

  def withTaskIdentityPaths(paths: Set[String]): Config = copy(taskIdentityPaths = paths)

  def withB2xPath(path: String): Config = copy(b2xPath = Option(path))

  def withAllowUnknown(value: Boolean): Config = copy(allowUnknown = value)
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
        ),
      opt[String]("task-identity")
        .unbounded()
        .action((path, config) => config.copy(taskIdentityPaths = config.taskIdentityPaths + path))
        .text(
          "path to a task identity sidecar (JSON lines). Repeatable. Each record ties one flattened task " +
            "declaration (file, line, task) to its authored ruleflow uuid; matched tasks are scoped exactly " +
            "like --rfl-src and the sidecar wins over .rfl inference."
        ),
      opt[String]("b2x")
        .action((path, config) => config.withB2xPath(path))
        .text(
          "path to the archive's BOM-to-XOM mapping (RULES_ENGINE/default/resources/ruleset/b2x.b2x). Its ARL " +
            "method bodies resolve the effects of calls the ARL itself only names."
        ),
      opt[Unit]("allow-unknown")
        .action((_, config) => config.withAllowUnknown(true))
        .text(
          "keep going when a construct lowers to an UNKNOWN node or a file has syntax errors. The CPG is then " +
            "tainted: every gap is still recorded as a FINDING. Without this flag such a build exits non-zero."
        )
    )
  }
}

object Main extends X2CpgMain(new Arl2Cpg(), cmdLineParser)
